#! /usr/bin/chez-scheme --script

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (alsa pcm)
  (uade))

(define device "default")

(define block/frames 2048)

(define main
  (lambda (modfile)
    (let ([handle (snd-pcm-open device 'playback 0)]
          [uade-state (uade-new-state)]
          [framebuf (uade-malloc/frames block/frames)])
      (snd-pcm-set-params handle (snd-pcm-format 's16-le) (snd-pcm-access 'rw-interleaved) 2 (uade-get-sampling-rate uade-state) 1 500000)
      (uade-play uade-state modfile)
      (let loop ([frames-read (uade-read/frames uade-state framebuf block/frames)])
        (cond
          [(= frames-read 0)
           #t]
          [else
            ;; TODO check for underruns.
            (snd-pcm-writei handle framebuf frames-read)
            (loop (uade-read/frames uade-state framebuf block/frames))]))
      (snd-pcm-drain handle)
      (uade-stop uade-state)
      (snd-pcm-close handle)
      (uade-free framebuf))))

(main (car (command-line-arguments)))
