(define-module (ssc improc kernels)
  #:use-module (ice-9 optargs)
  #:use-module (ssc maths convolve)
  #:export (dss-kernel))

(define (self-convolve filt n)
  ;; Sanity checking
  (if (not (= (array-rank filt) 1))
      (scm-error 'out-of-range #f
                 "Filter must have rank 1" '()
                 (list filter)))

  ;; Calculate the length of the result array. If the array goes from
  ;; a to b and is self-convolved n times, the output array will go
  ;; from n*a to n*b.
  (let* ((result-bounds (map (lambda (x) (* x (1+ n))) (car (array-shape filt))))
         (filt-A (make-typed-array (array-type filt) 0 result-bounds))
         (filt-B (make-typed-array (array-type filt) 0 result-bounds)))
    (array-copy! filt filt-A)
    (let lp ((A filt-A)
             (B filt-B)
             (i n))
      (if (= i 0) A
          (begin
            (convolve-array! A B filt 0)
            (lp B A (1- i)))))))

(define* (discrete-difference-kernel #:optional (order 1))
  (if (>= 0 order)
      #f64(1)
      (self-convolve #1f64@-1(1/2 0 -1/2) (1- order))))

(define* (discrete-gaussian-kernel #:optional (variance 1/3))
  (let ((T (inexact->exact (ceiling (* 3 variance)))))
    (if (>= 0 T)
        #f64(1)
        (self-convolve #1f64@-1(1/6 2/3 1/6) (1- T)))))

(define (dss-kernel variance order)
  (define smooth (discrete-gaussian-kernel variance))
  (define diff   (discrete-difference-kernel order))

  (let ((result (make-typed-array 'f64 0
                                  (map +
                                       (car (array-shape smooth))
                                       (car (array-shape diff))))))
    (convolve-array! smooth result diff 0)
    result))
