(library
  (uade ao)
  (export
    ao-play-buf
    )
  (import
    (rnrs)
    (uade ftypes-util)
    )

  (define load-library
    (load-shared-object "libao.so.4"))

  )
