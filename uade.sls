(library (uade)
  (export
    *uade-channels* *uade-bytes/sample* *uade-bytes/frame*

    uade-subsong-info uade-detection-info uade-song-info-t
    uade-song-info-module-bytes
    uade-song-info-module-md5
    uade-song-info-duration
    uade-song-info-subsong-bytes
    uade-song-info-song-bytes
    uade-song-info-module-fname
    uade-song-info-player-fname
    uade-song-info-format-name
    uade-song-info-module-name
    uade-song-info-player-name
    uade-song-info-subsong-current
    uade-song-info-subsong-min
    uade-song-info-subsong-default
    uade-song-info-subsong-max
    uade-song-info-custom
    uade-song-info-content
    uade-song-info-ext

    uade-malloc/frames uade-free
    uade-read/frames

    uade-new-state
    uade-play
    uade-read
    uade-stop
    uade-cleanup-state
    uade-get-fd
    uade-get-sampling-rate
    uade-get-song-info

    #;uade-read/bv
    )
  (import
    (chezscheme)
    (uade ftypes-util))

  (define load-lib
    (load-shared-object (locate-library-object "uade/libuade.so")))

  (define-ftype uade-state* void*)
  (define-ftype uade-config* void*)

  (define *uade-channels* 2)
  (define *uade-bytes/sample* 2)
  (define *uade-bytes/frame* (* *uade-channels* *uade-bytes/sample*))

  (define-ftype uade-subsong-info
    (struct
      [cur	int]
      [min	int]
      [def	int]
      [max	int]))

  (define-ftype uade-detection-info
    (struct
      [custom	boolean]
      [content	boolean]
      [ext	(array 16 unsigned-8)]
      [_	void*]))

  (define-ftype path-t (array 4096 unsigned-8))

  (define-ftype uade-song-info-t
    (struct
      [subsongs		uade-subsong-info]
      [detection-info	uade-detection-info]
      [module-bytes	size_t]
      [module-md5	(array 33 unsigned-8)]
      [duration		double]
      [subsong-bytes	integer-64]
      [song-bytes	integer-64]
      [module-fname	path-t]
      [player-fname	path-t]
      [format-name	(array 256 unsigned-8)]
      [module-name	(array 256 unsigned-8)]
      [player-name	(array 256 unsigned-8)]))

  (define uade-song-info-module-bytes
    (lambda (usi)
      (ftype-ref uade-song-info-t (module-bytes) usi)))

  (define uade-song-info-module-md5
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (module-md5) usi))))

  (define uade-song-info-duration
    (lambda (usi)
      (ftype-ref uade-song-info-t (duration) usi)))

  (define uade-song-info-subsong-bytes
    (lambda (usi)
      (ftype-ref uade-song-info-t (subsong-bytes) usi)))

  (define uade-song-info-song-bytes
    (lambda (usi)
      (ftype-ref uade-song-info-t (song-bytes) usi)))

  (define uade-song-info-module-fname
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (module-fname) usi))))

  (define uade-song-info-player-fname
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (player-fname) usi))))

  (define uade-song-info-format-name
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (format-name) usi))))

  (define uade-song-info-module-name
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (module-name) usi))))

  (define uade-song-info-player-name
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (player-name) usi))))

  (define uade-song-info-subsong-current
    (lambda (usi)
      (ftype-ref uade-song-info-t (subsongs cur) usi)))

  (define uade-song-info-subsong-min
    (lambda (usi)
      (ftype-ref uade-song-info-t (subsongs min) usi)))

  (define uade-song-info-subsong-default
    (lambda (usi)
      (ftype-ref uade-song-info-t (subsongs def) usi)))

  (define uade-song-info-subsong-max
    (lambda (usi)
      (ftype-ref uade-song-info-t (subsongs max) usi)))

  (define uade-song-info-custom
    (lambda (usi)
      (ftype-ref uade-song-info-t (detection-info custom) usi)))

  (define uade-song-info-content
    (lambda (usi)
      (ftype-ref uade-song-info-t (detection-info content) usi)))

  (define uade-song-info-ext
    (lambda (usi)
      (u8*->string (ftype-&ref uade-song-info-t (detection-info ext) usi))))

  ;; ALSA pcm lib works mainly in frames so define size using that unit.
  (define uade-malloc/frames
    (lambda (frame-count)
      (make-ftype-pointer unsigned-8 (foreign-alloc (* frame-count *uade-bytes/frame*)))))

  (define uade-free
    (lambda (framebuf)
      (foreign-free (ftype-pointer-address framebuf))
      (unlock-object framebuf)))

  (c-function
    [uade_new_state (uade-config*) uade-state*]
    [uade_play (string int uade-state*) int]
    [uade_read ((* unsigned-8) size_t uade-state*) ssize_t]
    [uade-stop (uade-state*) int]
    [uade-cleanup-state (uade-state*) void]
    [uade-get-fd (uade-state*) int]
    [uade-get-sampling-rate (uade-state*) int]
    [uade-get-song-info (uade-state*) (* uade-song-info-t)])

  (define uade-new-state
    (case-lambda
      [()
       (uade-new-state 0)]
      [(state)
       (let ([st (uade_new_state state)])
         (if (= st 0)
             (error #f "error")
             st))]))

  (define uade-play
    (case-lambda
      [(state fname)
       (uade-play state fname -1)]
      [(state fname subsong)
       (let ([rc (uade_play fname subsong state)])
         (cond
           [(> rc 0)
            rc]
           [(= rc 0)
            (error #f "song cannot be played" fname subsong)]
           [else
             (error #f "uade fatal error" fname subsong)]))]))

  ;; Expose an interface where uade-state is the first argument. Using syntax-rules is an optimisation so
  ;; the args are re-arranged at compile time.
  (define-syntax uade-read
    (syntax-rules ()
      [(_ state bufptr bufsize)
       (let ([rc (uade_read bufptr bufsize state)])
         (cond
           [(>= rc 0)
            rc]
           [else
            (error 'uade-read "error" rc)]))]))

  ;; ALSA pcm lib works mainly in frames so include a version using frames as the unit.
  (define-syntax uade-read/frames
    (syntax-rules ()
      [(_ state framebuf frame-count)
       (/ (uade-read state framebuf (* frame-count *uade-bytes/frame*)) *uade-bytes/frame*)]))

  #;(define uade-read/bv
    (case-lambda
      [(state)
       (uade-read/bv state (* *uade-bytes/frame* 1024))]
      [(state sz)
       (alloc ([buf &buf unsigned-8 sz])
          (let ([count (uade-read &buf sz state)])
            (if (= count 0)
                ;; FIXME: this is ugly. Returning an empty list so client code can use a (null?) check.
                ;; FIXME: sadly there's no bytevector-null? equivalent.
                '()
                (u8*->bv buf count))))]))
  )
