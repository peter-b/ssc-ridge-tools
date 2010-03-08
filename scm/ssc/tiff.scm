(define-module (ssc tiff))

(define tiffio-loaded #f)

(define (load-tiffio-extension)
  (if tiffio-loaded
      #t
      (begin
        (load-extension "libguile-tiffio" "init_tiffio")
        (set! tiffio-loaded #t)
        #t)))

(define-public (load-tiff filename)
  (load-tiffio-extension)
  (%load-tiff filename))
(define-public (save-tiff array filename)
  (load-tiffio-extension)
  (%save-tiff filename))
