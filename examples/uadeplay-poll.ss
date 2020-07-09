#! /usr/bin/chez-scheme --script

(debug-on-exception #t)

;; This example uses poll and snd-pcm-avail-update to fill the ALSA PCM ring buffer to exactly what it needs.

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (alsa pcm)
  (uade))

(define device "default")

(define buffer/frames #f)
(define period/frames #f)

(define configure-hw-params
  (lambda (handle uade-state)
    (let ([hwp (snd-pcm-hw-params-mallocz)])
      (snd-pcm-hw-params-any handle hwp)
      (snd-pcm-hw-params-set-access handle hwp 'rw-interleaved)
      (snd-pcm-hw-params-set-format handle hwp 's16-le)
      (snd-pcm-hw-params-set-channels handle hwp *uade-channels*)
      (snd-pcm-hw-params-set-rate-near handle hwp (uade-get-sampling-rate uade-state))
      (snd-pcm-hw-params-set-buffer-time-near handle hwp 500000)
      (snd-pcm-hw-params-set-period-time-near handle hwp 100000)
      (set! buffer/frames (snd-pcm-hw-params-get-buffer-size hwp))
      (display "hw buffer size (frames): ")(display buffer/frames)(newline)
      (set! period/frames (snd-pcm-hw-params-get-period-size hwp))
      (display "hw period size (frames): ")(display period/frames)(newline)
      (snd-pcm-hw-params handle hwp)
      (snd-pcm-hw-params-free hwp))))

(define configure-sw-params
  (lambda (handle)
    (let ([swp (snd-pcm-sw-params-mallocz)]
          [period-event #f])
      (snd-pcm-sw-params-current handle swp)
      (snd-pcm-sw-params-set-period-event handle swp period-event)
      ;; Disable auto-play by setting auto-start position to greater than the buffer.
      ;; This means an snd-pcm-start call is needed to begin play.
      (snd-pcm-sw-params-set-start-threshold handle swp (+ buffer/frames 1))
      ;; Wake client up via poll fd (POLLOUT) when the ring buffer is this empty.
      ;; eg, buffer/frames - (2 * period/frames).
      ;; There's a balance between waking up too often and keeping the buffer near always full vs
      ;; waiting a bit longer and saving on processing, but waiting too long and running the chance
      ;; of a buffer underrun.
      (snd-pcm-sw-params-set-avail-min handle swp (- buffer/frames (* period/frames 2)))
      (snd-pcm-sw-params handle swp)
      (snd-pcm-sw-params-free swp))))

(define main
  (lambda (modfile)
    (let ([handle (snd-pcm-open device 'playback 'nonblock)]
          [uade-state (uade-new-state)])

      (configure-hw-params handle uade-state)
      (configure-sw-params handle)

      ;; setup frame buffer and poll descriptors.
      (let* ([framebuf (uade-malloc/frames buffer/frames)]
             [fd-count (snd-pcm-poll-descriptors-count handle)]
             [fds (snd-pcm-poll-descriptors-alloc handle fd-count)])

        ;; [proc] load-frames: loads frames into framebuf.
        ;; [return] the actual number of frames loaded into framebuf.
        ;; It helps juggle between the:
        ;; - max local memory buffer size,
        ;; - max available frame space in alsa-lib ring buffer,
        ;; - actual amount of frames loadable from the music file.
        (define load-frames
          (lambda ()
            (let ([max-frames (fxmin buffer/frames (snd-pcm-avail-update handle))])
              (uade-read/frames uade-state framebuf max-frames))))

        (snd-pcm-poll-descriptors handle fds fd-count)
        (display "pcm stream: ")(display (snd-pcm-stream handle))(newline)
        (display "pcm type ")(display (snd-pcm-type handle))(newline)
        (display "pollfd count: ")(display fd-count)(newline)
        (do ([i 0 (+ i 1)])
          ((= i fd-count))
          (display "  fd: ")(display (pollfd-fd fds i))
          (display " events: ")(display (poll-flags (pollfd-events fds i)))
          (display " revents: ")(display (pollfd-revents fds i))
          (newline))

        (display "pcm state: ")(display (snd-pcm-state handle))(newline)

        (uade-play uade-state modfile)
        ;; Prime the alsa ring buffer before play proper.
        (let ([n (load-frames)])
          (display "pre-loading uade frames: ")(display n)(newline)
          (snd-pcm-writei handle framebuf n))

        ;; Begin play loop.
        (display "pcm state: ")(display (snd-pcm-state handle))(newline)
        (snd-pcm-start handle)
        (display "pcm state: ")(display (snd-pcm-state handle))(newline)
        (let loop ()
          (poll fds fd-count -1)
          (let ([revs (snd-pcm-poll-descriptors-revents/flags handle fds fd-count)])
            (cond
              [(null? revs)
               (loop)]
              [(memq 'out revs)
               ;; we can write more frames to the buffer.
               (let ([frames-read (load-frames)])
                 ;;(display "# ")(display frames-read)(display " revs ")(display revs)(newline)
                 (cond
                   [(= frames-read 0)
                    (display "song end")(newline)
                    #t]
                   [else
                     (snd-pcm-writei handle framebuf frames-read)
                     (loop)]))]
              [(memq 'err revs)
               ;; TODO check for underruns.
               (display "pcm underrun detected! aborting.")(newline)]
              [else
                (display "unknown poll ")(display revs)(newline)
                (loop)])))
        (uade-free framebuf))
      (snd-pcm-drain handle)
      (display "pcm state: ")(display (snd-pcm-state handle))(newline)
      (uade-stop uade-state)
      (snd-pcm-close handle))))

(main (car (command-line-arguments)))
