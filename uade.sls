(library (uade)
  (export
    uade-new-state
    uade-play
    uade-read
    uade-stop
    uade-cleanup-state
    )
  (import
    (chezscheme)
    (uade ftypes-util))

  (define load-lib
    (load-shared-object (locate-library-object "uade/libuade.so")))

  (define-ftype uade-state* void*)
  (define-ftype uade-config* void*)

  (c-function
    [uade-new-state (uade-config*) uade-state*]
    [uade-play (string int uade-state*) int]
    [uade-read ((* unsigned-8) size_t uade-state*) ssize_t]
    [uade-stop (uade-state*) int]
    [uade-cleanup-state () void])
  )
