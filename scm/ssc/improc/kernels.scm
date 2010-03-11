(define-module (ssc improc kernels)
  #:use-module (ice-9 optargs)
  #:use-module (ssc maths convolve)
  #:export (discrete-difference-kernel discrete-gaussian-kernel))

(define (discrete-difference-kernel)
  #1f64@-1(1/2 0 -1/2))

(define* (discrete-gaussian-kernel #:optional (variance 1/3))
  (let* ((T (inexact->exact (ceiling (* 3 variance))))
         (len (+ 3 (* 2 (1- T))))
         (ofs (- (floor (/ len 2))))
         (filtA (make-typed-array 'f64 0
                                  (list ofs (+ len ofs))))
         (filtB (make-typed-array 'f64 *unspecified*
                                  (list ofs (+ len ofs))))
         (kernel #1f64@-1(1/6 2/3 1/6)))

    (array-copy! kernel filtA)
    (let lp ((A filtA)
             (B filtB)
             (i T))
      (if (= i 1)
          A
          (begin
            (convolve-array! A B kernel 0)
            (lp B A (1- i)))))))
