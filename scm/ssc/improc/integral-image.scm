(define-module (ssc improc integral-image)
  #:use-module (ice-9 optargs)
  #:use-module (ssc arrays)
  #:export (make-integral-image box-integral apply-box-filter!))

(define (array-ref-or-0 array . indices)
  (if (apply array-in-bounds? array indices)
      (apply array-ref array indices)
      0))

(define (make-integral-image XX)
  (define II (apply make-typed-array
                    (array-type XX) 0
                    (array-dimensions XX)))
  (define (integral-at-point x y)
    (+ (array-ref XX x y)
       (array-ref-or-0 II (1- x) y)
       (array-ref-or-0 II x (1- y))
       (- (array-ref-or-0 II (1- x) (1- y)))))
  (array-index-map-in-order! II integral-at-point)
  II)

(define (box-integral II box-lower box-upper)
  (let ((II-upper (map cadr (array-shape II))))
    (let ((x1 (min (1- (car box-lower)) (car II-upper)))
          (y1 (min (1- (cadr box-lower)) (cadr II-upper)))
          (x2 (min (car box-upper) (car II-upper)))
          (y2 (min (cadr box-upper) (cadr II-upper))))
      (- (+ (array-ref-or-0 II x2 y2)
            (array-ref-or-0 II x1 y1))
         (+ (array-ref-or-0 II x1 y2)
            (array-ref-or-0 II x2 y1))))))

(define* (apply-box-filter! src-array dest-array
                            #:key (mapfunc list)
                            #:rest box-specs)
  (let* ((dest-bounds (array-shape dest-array))
         (dest-lower (map car dest-bounds))
         (dest-upper (map cadr dest-bounds))
         (src-lower (apply mapfunc dest-lower))
         (src-upper (apply mapfunc dest-upper))
         (stride (map / (map - src-upper src-lower)
                        (map - dest-upper dest-lower)))
         (offset (map (lambda (src dest stride) (- src (* dest stride)))
                      src-lower dest-lower stride)))

    (define (box-integral-at-point point spec)
      (let ((lower-bounds (list-ref spec 0))
            (upper-bounds (list-ref spec 1)))
        (box-integral src-array
                      (map + point lower-bounds)
                      (map + point upper-bounds))))

    (define (box-filter-at-point . dest-point)
      (let ((src-point (map (lambda (d s o) (+ (* d s) o))
                            dest-point stride offset)))

        (let lp ((v 0)
                 (lst box-specs))
          (if (null? lst)
              v
              (lp (+ v (box-integral-at-point src-point (car lst)))
                  (cdr lst))))))

    (array-index-map-in-order! dest-array box-filter-at-point)
    dest-array))
