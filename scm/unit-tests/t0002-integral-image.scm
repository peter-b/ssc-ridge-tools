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
  (assert-equal 4 (box-integral I '(0 0) '(1 1)))
  (assert-equal 4 (box-integral I '(-1 -1) '(1 1)))
  (assert-equal 4 (box-integral I '(0 0) '(2 2))))

(begin-test 'apply-box-integral!
  (define II (make-integral-image (make-array 1 3 3)))
  (let ((D (make-array 0 3 3))
        (expected #2((4 6 4) (6 9 6) (4 6 4))))
    (assert-equal expected
                  (apply-box-filter! II D '((-1 -1) (1 1)))))
  (let ((D (make-array 0 3 3))
        (filtspec '(((-1 -1) (-1 1)) ((1 -1) (1 1))))
        (expected #2((2 3 2) (4 6 4) (2 3 2))))
    (assert-equal expected (apply apply-box-filter! II D
                                  filtspec))))
