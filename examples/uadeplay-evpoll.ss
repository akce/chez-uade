#! /usr/bin/chez-scheme --script

;; This is an example of how a chez-libev ev-io (pollfd) based player could be written.
;;
;; It's less efficient than the evtimer based example as the alsa pcm poll file descriptors
;; seem to generate more events than would be suggested by snd-pcm-sw-params-set-avail-min.
;;
;; Presumably snd-pcm-poll-descriptors-revents would convert some of those to null events,
;; but even that is less elegant than using timers IMHO.

(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (alsa pcm)
  (ev)
  (uade))

(define device "default")

(define buffer/frames #f)
(define period/frames #f)

(define dump-song
  (lambda (uade-state)
    ;; Dump song info.
    (let ([inf (uade-get-song-info uade-state)])
      (display "sampling rate: ")(display (uade-get-sampling-rate uade-state))(newline)
      (display "module bytes: ")(display (uade-song-info-module-bytes inf))(newline)
      (display "duration: ")(display (flonum->fixnum (uade-song-info-duration inf)))(newline)
      (display "module filename: ")(display (uade-song-info-module-fname inf))(newline)
      (display "player filename: ")(display (uade-song-info-player-fname inf))(newline)
      (display "format name: ")(display (uade-song-info-format-name inf))(newline)
      (display "module name: ")(display (uade-song-info-module-name inf))(newline)
      (display "player name: ")(display (uade-song-info-player-name inf))(newline)
      (display "subsong cur: ")(display (uade-song-info-subsong-current inf))(newline)
      (display "subsong default: ")(display (uade-song-info-subsong-default inf))(newline)
      (display "subsong min: ")(display (uade-song-info-subsong-min inf))(newline)
      (display "subsong max: ")(display (uade-song-info-subsong-max inf))(newline)
      (display "custom: ")(display (uade-song-info-custom inf))(newline)
      (display "content: ")(display (uade-song-info-content inf))(newline)
      (display "ext: ")(display (uade-song-info-ext inf))(newline)
      (display "total frames: ")(display (flonum->fixnum (* (uade-song-info-duration inf) (uade-get-sampling-rate uade-state))))(newline))))

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
          [period-event #t])
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
      (snd-pcm-sw-params-set-avail-min handle swp (if period-event
                                                      buffer/frames
                                                      (- buffer/frames (* period/frames 2))))
      (when period-event
        (snd-pcm-sw-params-set-period-event handle swp period-event))
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
        (define play-watcher #f)
        (define stdin-watcher #f)
        (define song-duration/seconds #f)
        (define POLLOUT #f)

        ;; [proc] load-frames: loads frames into framebuf.
        ;; [return-values] the number of frames requested plus the actual number of frames loaded into framebuf.
        ;; It helps juggle between the:
        ;; - max local memory buffer size,
        ;; - max available frame space in alsa-lib ring buffer,
        ;; - actual amount of frames loadable from the music file.
        (define load-frames
          (lambda ()
            (let ([max-frames (fxmin buffer/frames (snd-pcm-avail-update handle))])
              (values max-frames (uade-read/frames uade-state framebuf max-frames)))))

        (define bytes/second (* *uade-bytes/frame* (uade-get-sampling-rate uade-state)))

        (define current-pos/seconds
          (lambda ()
            (flonum->fixnum
              (real->flonum
                (/ (uade-song-info-subsong-bytes (uade-get-song-info uade-state))
                   bytes/second)))))

        (define play-frame
          (lambda (watcher revents)
            ;; we can write more frames to the buffer.
            (let-values ([(max-frames frames-read) (load-frames)])
              (define p (current-pos/seconds))
              (display "pos ")(display p)
              (display "s loaded frame ")(display frames-read)(display "     ")(display "\r")
              ;; frames-read can equal zero, sometimes due to the rapidity of poll fd events.
              (when (> frames-read 0)
                 ;; TODO check for underruns.
                 (snd-pcm-writei handle framebuf frames-read))
              ;; uade-read always fills the buffer to the requested size, unless its run out of song
              ;; so use that property to check for end-of-song.
              (when (< frames-read max-frames)
                  (newline)
                  (display "song end")(newline)
                  (ev-io-stop watcher)
                  (ev-io-stop stdin-watcher)))))

        (define pcm-event
          (lambda (watcher revents)
            (let ([revs (evmask revents)])
              (when (memq POLLOUT revs)
                (play-frame watcher revs)))))

        (define set-pollfd-evflags!
          (lambda (fd)
            (let ([pf (poll-flags fd)])
              (cond
                [(memq 'in pf)
                 (set! POLLOUT 'READ)]
                [(memq 'out pf)
                 (set! POLLOUT 'WRITE)]
                [else
                  (error #f "unknown poll mode" pf)]))))

        (set! stdin-watcher
          (ev-io 0 (evmask 'READ)
                 (lambda (w rev)
                   (let ([ch (read-char)])
                     (newline)(display "key pressed: ")(display ch)(newline)
                     (case ch
                       [(#\i)
                        (dump-song uade-state)]
                       [(#\q)
                        (ev-io-stop w)
                        (ev-io-stop play-watcher)])))))

        ;; Initialise poll file descriptors.
        (snd-pcm-poll-descriptors handle fds fd-count)
        (display "pcm stream: ")(display (snd-pcm-stream handle))(newline)
        (display "pcm type ")(display (snd-pcm-type handle))(newline)
        (display "pollfd count: ")(display fd-count)(newline)
        (do ([i 0 (+ i 1)])
          ((= i fd-count))
          ;; This should be safe so long as every fd belongs to the same snd-pcm-type.
          (set-pollfd-evflags! (pollfd-events fds i))
          (set! play-watcher (ev-io (pollfd-fd fds i) (evmask POLLOUT) pcm-event))
          (display "  fd: ")(display (pollfd-fd fds i))
          (display " events: ")(display (poll-flags (pollfd-events fds i)))
          (display " revents: ")(display (pollfd-revents fds i))
          (newline))

        (display "pcm state: ")(display (snd-pcm-state handle))(newline)

        (uade-play uade-state modfile)
        ;; Prime the alsa ring buffer before play proper.
        (let-values ([(_ n) (load-frames)])
          (display "pre-loading uade frames: ")(display n)(newline)
          (snd-pcm-writei handle framebuf n))

        (set! song-duration/seconds
          (flonum->fixnum (uade-song-info-duration (uade-get-song-info uade-state))))

        ;; Begin play loop.
        (display "pcm state: ")(display (snd-pcm-state handle))(newline)
        (snd-pcm-start handle)
        (display "pcm state: ")(display (snd-pcm-state handle))(newline)
        (display "press 'i' for song info, 'q' to quit player.")(newline)
        (ev-run)
        (uade-free framebuf))
      (snd-pcm-drain handle)
      (display "ending pcm state: ")(display (snd-pcm-state handle))(newline)
      (uade-stop uade-state)
      (snd-pcm-close handle))))

(main (car (command-line-arguments)))
