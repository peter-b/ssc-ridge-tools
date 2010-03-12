(define-module (ssc arrays)
  #:use-module (srfi srfi-1)
  #:export (array-index-map-in-order!))

;;;; array-index-for-each array proc
;;
;; Apply proc to each index in array. The vaue returned is
;; unspecified. Each call is `(PROC I1 ... IN)', where I1...IN is an
;; index into ARRAY.
(define (array-index-for-each array proc)
  (define (map-recursive indices shape)
    (if (null? shape)
        (apply proc indices)
        (let ((dim-bounds (car shape)))
          (let ((lower (car dim-bounds))
                (upper (cadr dim-bounds)))
            (do ((i lower (1+ i)))
                ((> i upper)
                 array)
              (map-recursive (cons i indices) (cdr shape)))))))
    (map-recursive '() (reverse (array-shape array))))

;;;; array-index-map-in-order! array proc
;;
;; See documentation for array-index-map! for usage. The main
;; difference is that this procedure makes guarantees about the order
;; that array indices are visited. Specifically, when traversing an
;; n-dimensional array bounded by the points A = (a1, a2, ..., an) and
;; B = (b1, b2, ..., bn), this procedure guarantees that when visiting
;; a point X = (x1, x2, ..., xn) all points in the n-rectangle bounded
;; by A and (x1 - 1, x2 - 1, ..., xn - 1) will have been previously
;; visited.
(define (array-index-map-in-order! array proc)
  (array-index-for-each
   array
   (lambda indices
     (apply array-set! array (apply proc indices) indices))))
