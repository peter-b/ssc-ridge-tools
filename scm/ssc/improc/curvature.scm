(define-module (ssc improc curvature)
  #:export (curvatures))

;;;; Calculate the principal curvatures and principal directions
;;
;; Calculates the eigenvalues and eigenvectors of the symmetric
;; Hessian matrix defined by the discrete second-order differences
;; Lxx, Lyy, Lxy.
;;
;; Returns ((Lqq vq1 vq2) (Lpp vp1 vp2))
(define (curvatures Lxx Lyy Lxy)
  (define (norm-vector v)
    (let ((norm-factor (/ (sqrt (apply + (map (lambda (x) (* x x)) v))))))
      (map (lambda (x) (* x norm-factor)) v)))
  ;; Calculate principle curvatures Lpp and Lqq by taking the
  ;; eigenvalues of the symmetric Hessian matrix.
  (let* ((A (/ (+ Lxx Lyy) 2))
         (dL (- Lxx Lyy))
         (B (/ (sqrt (+ (* dL dL) (* 4 Lxy Lxy))) 2))
         (Lpp (+ A B))
         (Lqq (- A B)))
    ;; Calculate eigenvectors
    (let* ((vpp (norm-vector
                 (if (= 0 Lxy)
                     '(1 0)
                     (let ((ev-coef (/ (- Lpp Lxx) Lxy)))
                       (if (> 1 (abs ev-coef))
                           (list (/ ev-coef) 1)
                           (list 1 ev-coef))))))
           (vqq (list (- (cadr vpp)) (car vpp))))
      (list (cons Lpp vpp) (cons Lqq vqq)))))
