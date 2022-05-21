#lang rosette

; Another solution to connectivity using Lists of Lists, to prevent index problems

; ==== HELPERS ====

; (Listof Integer) -> (Listof bool)
(define (int->bool m)
  (map (λ (i) (positive? i)) m))

; (Listof ?) Integer -> (Listof (Listof ?))
(define (unflatten flat num-vert)
  (for/list ([i (in-range num-vert)])
    (define-values (Thead Ttail) (split-at flat (* i num-vert)))
    (take Ttail num-vert)))

; ==== COMPUTATION ====

; (Listof Bool) Natural -> (Listof SymbolicBoolean)
(define (connectivity T-one num-vert)
  (define T (unflatten T-one num-vert))
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
           ;(set! k-list (cons k k-list))
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


(module+ test
  (require rackunit)

  ; checks that the only solution to (connectivity input) is the expected matrix
  ; (Listof Integer) (Listof Integer) -> Bool
  (define-simple-check (exact-solution? input expected)
    (result-value ; extract the final value
     (with-vc (vc) ; isolate each test case
       (begin
         (define input-tree (int->bool input))
         (define num-vert (sqrt (length input-tree)))
         (define actual (connectivity input-tree num-vert))
         (define expected-bools (unflatten (int->bool expected) num-vert))
         ; solve for any solution
         (define M (solve (assert #t)))
         ; solve for a solution which doesn't match the expected
         (define Mnot (verify (assert (equal? actual
                                              expected-bools))))
         (cond [(and (not (unsat? M))
                     (equal? (evaluate actual M) expected-bools)
                     (unsat? Mnot))
                (displayln "ok")
                #t] 
               [else ; print out errors
                (displayln (format "input ~a" input))
                (when (unsat? M) (displayln "no solution found"))
                (unless (unsat? Mnot)
                  (displayln "other solution found:")
                  (define (Medge i j) (Mnot (list-ref (list-ref actual i) j)))
                  (for ([i (in-range num-vert)])
                    (for ([j (in-range num-vert)])
                      (display (if (Medge i j)
                                   1 0)))
                    (display "\n")))
                #f])))))

  (exact-solution?
   (list 0 0 1 0
         0 0 0 1
         1 0 0 0
         0 1 0 0)
   (list 1 0 1 0
         0 1 0 1
         1 0 1 0
         0 1 0 1))
         
    
  (exact-solution?
   (make-list 16 0)
   (list 1 0 0 0
         0 1 0 0
         0 0 1 0
         0 0 0 1))

  (exact-solution?
   (list 0 1 0 0 ; 1-2-3
         1 0 1 0
         0 1 0 0
         0 0 0 0)
   (list 1 1 1 0
         1 1 1 0
         1 1 1 0
         0 0 0 1))

  (exact-solution?
   (list 0 1 0 0 ; 1-2, 3-4
         1 0 0 0
         0 0 0 1
         0 0 1 0)
   (list 1 1 0 0
         1 1 0 0
         0 0 1 1
         0 0 1 1))

  (exact-solution?
   (list 0 1 0 0 1 ; 1-2-5, 3-4
         1 0 0 0 0
         0 0 0 1 0
         0 0 1 0 0
         1 0 0 0 0)
   (list 1 1 0 0 1
         1 1 0 0 1
         0 0 1 1 0
         0 0 1 1 0
         1 1 0 0 1))


  ; 6x6 grid. time ≈ 20 seconds 
  (exact-solution?
   (list 0 1 0 0 0 0 
         1 0 1 0 0 0 
         0 1 0 1 0 0 
         0 0 1 0 0 0 
         0 0 0 0 0 1 
         0 0 0 0 1 0)
   (list 1 1 1 1 0 0 
         1 1 1 1 0 0 
         1 1 1 1 0 0 
         1 1 1 1 0 0 
         0 0 0 0 1 1 
         0 0 0 0 1 1))

  ) 