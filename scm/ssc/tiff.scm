(define-module (ssc tiff)
  #:export (save-tiff load-tiff))

;;;; Allow dynamic library to be loaded lazily
(define %tiff-module (current-module))
(define (load-tiff-extension)
  (save-module-excursion
   (lambda ()
     (set-current-module %tiff-module)
     (load-extension "libguile-tiffio" "init_tiffio"))))


(define (load-tiff . args)
  (if (defined? '%load-tiff)
      #t
      (load-tiff-extension))
  (apply %load-tiff args))

(define (save-tiff . args)
  (if (defined? '%save-tiff)
      #t
      (load-tiff-extension))
  (apply %save-tiff args))
