#! /usr/bin/chez-scheme --script

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (alsa pcm)
  (uade))

(define device "default")

(define block/frames 2048)
(define block/bytes (* *uade-bytes/frame* block/frames))

(define main
  (lambda (modfile)
    (let ([handle (snd-pcm-open device (snd-pcm-stream 'playback) 0)]
          [uade-state (uade-new-state)])
      (snd-pcm-set-params handle (snd-pcm-format 's16-le) (snd-pcm-access 'rw-interleaved) 2 (uade-get-sampling-rate uade-state) 1 500000)
      (uade-play modfile -1 uade-state)
      (let loop ([bv (uade-read/bv uade-state block/bytes)])
        (cond
          [(null? bv)
           #t]
          [else
            ;; TODO check for underruns.
            (snd-pcm-writei/bv handle bv (/ (bytevector-length bv) *uade-bytes/frame*))
            (loop (uade-read/bv uade-state block/bytes))]))
      (snd-pcm-drain handle)
      (uade-stop uade-state)
      (snd-pcm-close handle))))

(main (car (command-line-arguments)))
