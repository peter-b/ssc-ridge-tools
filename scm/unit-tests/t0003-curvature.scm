(use-modules (ssc improc curvature))
(use-modules (srfi srfi-1))

(begin-test 'curvatures
  (define (compare-curvature-results A B)
    (let ((cfunc (lambda (a b p) (and p (= a b)))))
      (or
       (and
        (fold cfunc #t (car A) (car B))
        (fold cfunc #t (cadr A) (cadr B)))
       (and
        (fold cfunc #t (car A) (cadr B))
        (fold cfunc #t (cadr A) (car B))))))
  (assert-equal '((1 1 0) (1 0 1)) (curvatures 1 1 0)
                #:equal-func? compare-curvature-results)
  (assert-equal (let ((irt2 (/ (sqrt 2))))
                  (list (list -1 (- irt2) irt2)
                        (list 1 irt2 irt2)))
                (curvatures 0 0 1)
                #:equal-func? compare-curvature-results))
