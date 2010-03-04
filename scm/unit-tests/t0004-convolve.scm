(use-modules (ssc maths convolve))

(begin-test 'convolve-array!
  (define A #(0 1 0 0))
  (define B #(0 0 0 0))
  (define f #1@-1(1 1 1))
  (convolve-array! A B f 0)
  (assert-equal #(1 1 1 0) B))

(begin-test 'convolve-array!-2
  (define A #2((0 0 0) (0 1 0) (0 0 0)))
  (define B #2((0 0 0) (1 1 1) (0 0 0)))
  (define C #2((0 0 0) (0 0 0) (0 0 0)))
  (define f #1@-1(1 1 1))
  (convolve-array! A C f 1)
  (assert-equal B C)
  (convolve-array! A C f 0)
  (assert-equal (transpose-array B 1 0) C))
