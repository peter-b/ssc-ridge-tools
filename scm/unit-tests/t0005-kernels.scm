(use-modules (ssc improc kernels))

;; This test probably needs to be expanded.
(begin-test 'dss-kernel
  (assert-equal #f64@-1(1/6 2/3 1/6) (dss-kernel 1/3 0))
  (assert-equal #f64@-1(1/2 0 -1/2) (dss-kernel 0 1)))
