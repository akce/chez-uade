(library (uade)
  (export
    *uade-channels* *uade-bytes/sample* *uade-bytes/frame*

    uade-new-state
    uade-play
    uade-read
    uade-stop
    uade-cleanup-state
    uade-get-fd
    uade-get-sampling-rate

    uade-read/bv
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

  (c-function
    [uade_new_state (uade-config*) uade-state*]
    [uade-play (string int uade-state*) int]
    [uade-read ((* unsigned-8) size_t uade-state*) ssize_t]
    [uade-stop (uade-state*) int]
    [uade-cleanup-state () void]
    [uade-get-fd (uade-state*) int]
    [uade-get-sampling-rate (uade-state*) int])

  (define uade-new-state
    (case-lambda
      [()
       (uade-new-state 0)]
      [(state)
       (uade_new_state state)]))

  (define uade-read/bv
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
