(define-module (ssc arrays)
  #:use-module (srfi srfi-1)
  #:export (array-index-map-in-order!))

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
  (define B (array-shape array))

  ;; Calculates the indices of the next entry to run proc for. Returns
  ;; #f if all the locations in the array have been visited.
  (define (next-indices! current)
    ;; ilst is the current offset into the indices list, and blst is
    ;; the current offset into the bounds list.
    (let lp ((ilst current)
             (blst B))
      (if (null? ilst)
          ;; If we get to the end of the indices list without finding
          ;; an index that we're able to increment, we must be
          ;; finished.
          #f
          ;; If the current index is at its upper bound, set it to its
          ;; lower bound and carry on to the next index. Otherwise,
          ;; increment it, and return the modified indices.
          (let ((save-idx (car ilst)))
            (if (= save-idx (cadr (car blst)))
                (begin
                  (set-car! ilst (car (car blst)))
                  (lp (cdr ilst) (cdr blst)))
                (begin
                  (set-car! ilst (1+ (car ilst)))
                  current))))))

  (let lp ((i (map car B))) ; Initialise the indices to all lower
                            ; bounds.
    (apply array-set! array (apply proc i) i)
    (if (next-indices! i)
        (lp i)
        #t)))
