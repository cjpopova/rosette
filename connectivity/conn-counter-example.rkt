#lang rosette

; (Listof Bool) Natural -> (Listof SymbolicBoolean)
(define (conn-correct T num-vert)
  ; adjacency matrix
  (define-symbolic* G boolean? #:length (length T))

  ; Macro from Rosette guide
  (define fuel (make-parameter num-vert))
  (define-syntax-rule
    (define-bounded (id param ...) body ...)
    (define (id param ...)
      (assert (> (fuel) 0) "Out of fuel.")
      (parameterize ([fuel (sub1 (fuel))])
        body ...)))

  ; edge accessors
  (define (Tedge i j) (list-ref T (+ (* i num-vert) j)))
  (define (Gedge i j) (list-ref G (+ (* i num-vert) j)))

  ; recursive predicate
  (define-bounded (path? u v)
    (cond [(= u v) #t]
          [(Tedge u v) #t]
          [(Tedge v u) #t]
          [else
           (define-symbolic* k integer?)
           (exists (list k)
                   (and
                    ;(0 . <= . k)
                    ;(k . < . num-vert)
                    (Tedge u k)
                    (Tedge k u)
                    (path? k v)))]))

  (for* ([i (in-range num-vert)]
         [j (in-range num-vert)])
    (assert (equal? (path? i j) (Gedge i j))))

  G)

(define k-list '())

(define (conn-modified T-one num-vert)
  (define T
    (for/list ([i (in-range num-vert)])
      (define-values (Thead Ttail) (split-at T-one (* i num-vert)))
      (take Ttail num-vert)))
  ; adjacency matrix
  (define G
    (for/list ([i (in-range num-vert)])
      (define-symbolic* Grow boolean? #:length (length T))
      Grow))

  ; Macro from Rosette guide
  (define fuel (make-parameter num-vert))
  (define-syntax-rule
    (define-bounded (id param ...) body ...)
    (define (id param ...)
      (assert (> (fuel) 0) "Out of fuel.")
      (parameterize ([fuel (sub1 (fuel))])
        body ...)))

  ; edge accessors
  (define (Tedge i j) (list-ref (list-ref T i) j))
  (define (Gedge i j) (list-ref (list-ref G i) j))

  ; recursive predicate
  (define-bounded (path? u v)
    (cond [(= u v) #t]
          [(Tedge u v) #t]
          [(Tedge v u) #t]
          [else
           (define-symbolic* k integer?)
           (set! k-list (cons k k-list))
           (exists (list k)
                   (and
                    ; no index assertions
                    (Tedge u k)
                    (Tedge k u)
                    (path? k v)))]))

  (for* ([i (in-range num-vert)]
         [j (in-range num-vert)])
    (if (i . <= . j)
        (assert (equal? (path? i j) (Gedge i j)))
        (assert (equal? (Gedge i j) (Gedge j i)))))

  G)


; ==== COUNTEREXAMPLE SEARCH ====

(define N 5)

(define-symbolic input boolean? #:length (* N N))

(for* ([i (in-range N)]
         [j (in-range i)])
    (assume (equal? (list-ref input (+ (* i N) j))
                    (list-ref input (+ (* j N) i)))))

(define correct-G (conn-correct input N))
(displayln "finished bounded")

(define modified-G (conn-modified input N))
(displayln "finished not bounded")

(time 
   (verify (equal? correct-G (flatten modified-G))))

#| NOTES
0 <= k < num-vert constraint ...... unnecessary
iterating over 1/2 the matrix in a triangle ... good
|#

