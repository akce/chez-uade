#! /usr/bin/chez-scheme --script

(debug-on-exception #t)

;; This example uses a libev timer and snd-pcm-avail-update to fill the ALSA PCM ring buffer to exactly what it needs.

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (alsa pcm)
  (ev)
  (uade))

(define device "default")

(define buffer/frames #f)
(define period/frames #f)
(define buffer-empty/percent 60)	; Let alsa audio buffer/frames get this empty before refilling.

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
    (let ([swp (snd-pcm-sw-params-mallocz)])
      (snd-pcm-sw-params-current handle swp)
      ;; Disable auto-play by setting auto-start position to greater than the buffer.
      ;; This means an snd-pcm-start call is needed to begin play.
      (snd-pcm-sw-params-set-start-threshold handle swp (+ buffer/frames 1))
      (snd-pcm-sw-params handle swp)
      (snd-pcm-sw-params-free swp))))

(define main
  (lambda (modfile)
    (let ([handle (snd-pcm-open device (snd-pcm-stream 'playback) (snd-open-mode 'nonblock))]
          [uade-state (uade-new-state)])

      (configure-hw-params handle uade-state)
      (configure-sw-params handle)

      ;; setup frame buffer and poll descriptors.
      (let* ([framebuf (uade-malloc/frames buffer/frames)])

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

        (define play-frame
          (lambda (watcher revents)
            ;; we can write more frames to the buffer.
            (let ([frames-read (load-frames)])
              (display "# ")(display frames-read)(display "\r")
              (cond
                [(> frames-read 0)
                 ;; TODO check for underruns.
                 (snd-pcm-writei handle framebuf frames-read)]
                [else
                 (display "song end")(newline)
                 (ev-timer-stop watcher)]))))

        ;; Calculate the libev timer wait-time (between 0 and 1) based on buffer size, song frame rate,
        ;; and how empty we want the buffer before refilling.
        (let ([wait-time
                (real->flonum
                  (/
                    (* buffer/frames (/ buffer-empty/percent 100))
                    (uade-get-sampling-rate uade-state)))])
          (ev-timer wait-time wait-time play-frame))

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
        (ev-run)
        (uade-free framebuf))
      (snd-pcm-drain handle)
      (display "pcm state: ")(display (snd-pcm-state handle))(newline)
      (uade-stop uade-state)
      (snd-pcm-close handle))))

(main (car (command-line-arguments)))