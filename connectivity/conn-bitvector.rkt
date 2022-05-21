#lang rosette

; an older version of connectivity with bitvector - didn't work probably because of index issues

(define dim 3)

(define-symbolic mat (bitvector (* dim dim))) ; matrix

; Parameter that controls the number of unrollings.
(define fuel (make-parameter dim))
; A simple macro for defining bounded procedures
; that use (fuel) to limit recursion.
(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Out of fuel.")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

; integer integer -> (bitvector 1)
(define (edge i j) (bit (+ (* i dim) j) mat))
; integer integer Model -> (Union 0 1)
(define (mxM i j M) (bit (+ (* i dim) j) (M mat)))

; integer integer -> boolean
(define-bounded (path i j)
  (cond [(= i j) #t]
        [else
         (define-symbolic* k integer?)
         (exists (list k)
                 (and (edge i k)
                      (edge k i)
                      (path k j)))]))

(for* ([i (in-range dim)]
       [j (in-range dim)])
  (assert (equal?
           (if (path i j) (bv 1 1) (bv 0 1))
           (edge i j)))
  #;(solver-assert
     (current-solver)
     (list (if (path i j)
               (not (bvzero? (mx i j)))
               (bvzero? (mx i j))))))

; ==== QUERY/SOLVE ====

(define M (solve (assert #t #;(bvzero? (edge 1 0)))))
M

; ==== PRINTING  ====
#;(for ([i (in-range dim)])
    (for ([j (in-range dim)])
      (display (bitvector->natural (mxM i j M))))
    (display "\n"))

; this probably shows bv backwards
#|(for ([i (in-range dim)])
  (define lower (* i dim))
  (define upper (+ lower (sub1 dim)))
  ;(displayln (format "~a ~a" lower upper))
  (displayln (extract upper lower (M mat))) ; notice ordering
  #;(display "\n"))|#
