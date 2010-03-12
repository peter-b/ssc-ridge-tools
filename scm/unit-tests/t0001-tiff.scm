(use-modules (ssc tiff))

(begin-test 'tiff-save-load
  (define tempfile #f)
  (define (gettempfile)
    (let* ((port (mkstemp! (string-copy "/tmp/tiff-test-XXXXXX")))
           (filename (port-filename port)))
      (close-port port)
      filename))
  (define A
    (let ((M (make-typed-array 'f32 0 256 256)))
      (let lp ((x 0)
               (y 0))
        (array-set! M 1 x y)
        (if (= y 255)
            (if (= x 255)
                M
                (lp (1+ x) 0))
            (lp x (1+ y))))))
  (dynamic-wind
      (lambda () (set! tempfile (gettempfile)))
      (lambda ()
        (assert-true (save-tiff A tempfile))
        (assert-equal A (load-tiff tempfile)))
      (lambda () (delete-file tempfile))))
