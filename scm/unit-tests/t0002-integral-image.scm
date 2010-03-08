(use-modules (ssc improc integral-image))

(begin-test 'make-integral-image
  (define A #2((1 1) (1 1)))
  (define B #2((1 2) (2 4)))
  (assert-true
   (array-equal? B (make-integral-image A))))

(begin-test 'box-integral
  (define I (make-integral-image (make-array 1 2 2)))
  (assert-equal 1 (box-integral I '(0 0) '(0 0)))
  (assert-equal 1 (box-integral I '(1 1) '(1 1)))
  (assert-equal 4 (box-integral I '(0 0) '(1 1))))
