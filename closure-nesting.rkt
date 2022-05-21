#lang rosette

(define input ; row = source. col = destination
  (list 0 1 1 0
        0 0 0 1
        0 0 0 0
        0 0 0 0))
(define expected ; row = closure. col = in-scope
  (list 1 0 0 0
        1 1 0 0
        1 0 1 0
        1 1 0 1))

; (Listof Integer) -> (Listof bool)
(define (int->bool m)
  (map (λ (i) (positive? i)) m))

(define tree (int->bool input))

; # of vertices
(define dim (sqrt (length input)))

(define-symbolic actual boolean? #:length (* dim dim)) ; matrix

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

; edge accessors
(define (Tedge i j) (list-ref tree (+ (* i dim) j)))
(define (Medge i j) (list-ref actual (+ (* i dim) j)))

(define-bounded (in-scope i j) ; is i in-scope in j?
  (cond [(= i j) #t]
        [else
         (define-symbolic* k integer?)
         (exists (list k)
                 (and (Tedge k j)
                      (in-scope i k)))]))

(for* ([i (in-range dim)]
       [j (in-range dim)])
  (cond [(Tedge j i)
         (assume (Medge i j))]
        [else
         (assert (equal? (in-scope j i) (Medge i j)))]))


; ==== QUERY/SOLVE ====

; test actual != expected
(define M (solve (assert (not (equal? actual
                                      (int->bool expected))))))

; ==== PRINTING ====
(for ([i (in-range dim)])
  (for ([j (in-range dim)])
    (display (if (M (Medge i j))
                 1 0)))
  (display "\n"))