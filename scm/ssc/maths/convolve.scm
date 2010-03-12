(define-module (ssc maths convolve)
  #:export (convolve-array!))

(define (convolve-1D! src src-start src-end
                      filter filter-start filter-end
                      dest dest-start dest-end)
  (define (conv-term n)
    (let ((mink (max src-start (- n filter-end)))
          (maxk (min src-end (- n filter-start))))
      (do ((k mink (1+ k))
           (v 0 (+ v (* (array-ref src k)
                        (array-ref filter (- n k))))))

          ((> k maxk) ; Stop condition

           v)))) ; Final return value

  (do ((n dest-start (1+ n)))
      ((> n dest-end))
    (array-set! dest (conv-term n) n))
  dest)

(define (convolve-array! src dest filter dimension)
  ;; Returns a list of dimensions that can be used with
  ;; transpose-array to make dimension the first dimension in each of
  ;; the arrays (this is helpful to know which dimension we're
  ;; applying the filter along).
  (define (transpose-dims array new-first-dim)
    (let ((r (array-rank array)))
      (let lp ((i 0)
               (dims (list new-first-dim)))
        (if (>= i r)
            (reverse! dims)
            (lp (1+ i)
                (if (= i new-first-dim) dims (cons i dims)))))))

  ;; Do some sanity checking
  (cond
   ((not (= (array-rank src) (array-rank dest)))
    (scm-error 'out-of-range #f
               "Destination rank ~A must match source rank ~A"
               (list (array-rank dest) (array-rank src))
               (list dest)))
   ((not (= (array-rank filter)))
    (scm-error 'out-of-range #f
               "Filter must have rank 1" (list (array-rank filter))
               (list filter))))

  ;; Set up a huge selection of parameters for the recursive
  ;; convolution.
  (let* ((t-dims (transpose-dims dest dimension))
         (src-T (apply transpose-array src t-dims))
         (dest-T (apply transpose-array dest t-dims))
         (src-shape (array-shape src-T))
         (dest-shape (array-shape dest-T))
         (filter-shape (array-shape filter))
         (src-start (list-ref (car src-shape) 0))
         (src-end (list-ref (car src-shape) 1))
         (dest-start (list-ref (car dest-shape) 0))
         (dest-end (list-ref (car dest-shape) 1))
         (filter-start (list-ref (car filter-shape) 0))
         (filter-end (list-ref (car filter-shape) 1)))

    (define (convolve-recursive indices shape)
      (if (null? shape)

          ;; If we've narrowed the position in the transposed arrays
          ;; down to a single column, create shared arrays mapping
          ;; onto those single columns and call convolve-1D! to do the
          ;; heavy lifting.
          (convolve-1D!
           (make-shared-array src-T
                              (lambda (i) (cons i indices))
                              (car src-shape))
           src-start src-end

           filter filter-start filter-end

           (make-shared-array dest-T
                              (lambda (i) (cons i indices))
                              (car dest-shape))
           dest-start dest-end)

          ;; Otherwise, narrow down the indices by another dimension
          (let ((dim-bounds (car shape)))
            (let ((lower (car dim-bounds))
                  (upper (cadr dim-bounds)))
              (do ((i lower (1+ i)))
                  ((> i upper)
                   dest)
                (convolve-recursive (cons i indices) (cdr shape)))))))

    ;; Actually do the convolution.
    (convolve-recursive '() (reverse (cdr dest-shape)))))


(define (convolve-array array filter dimension)
  (let ((result (apply make-typed-array
                       (array-type array)
                       *unspecified*
                       (array-shape array))))
    (convolve-array! array result filter dimension)
    result))
