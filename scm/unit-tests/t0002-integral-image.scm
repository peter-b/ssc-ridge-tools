(use-modules (ssc improc integral-image))

(begin-test 'make-integral-image
  (define A #2((1 1) (1 1)))
  (define B #2((1 2) (2 4)))
  (assert-true
   (array-equal? B (make-integral-image A))))
