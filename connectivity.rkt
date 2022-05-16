#lang rosette

#|
[Problem 1]
Let T = (V, P) be a tree with vertices V and edges P.

The edges P are symbolic. In the formula over P, we have the property that:
A model M will turn T into 1 or more connected components, where each component will be
- a single vertex, OR
- a set of vertices connected by a path

Let G = (V, E) be a complete graph over the same set of vertices V, and edges E.

The edges E are symbolic. We wish to make G representation of the cliques for each connected
component in T.

Base case: ∀ u, v ∈ V . (u v) ∈ P \implies (u v) ∈ E 
Inductive case: ∀ u, v ∈ V . u is connected to v in T \implies (u v) ∈ E

More formally:
∀ u, v ∈ V .
  (u v) ∈ E <=>
  (or (= u v)
      (u v) ∈ P
      ∃ k ∈ V . (u k) ∈ P ∧ (k v) ∈ E
                        
Challenge: The construction of T is out of scope of this problem. We are concerned with 
the construction of G.

Solution:
1 ~ Data structures. Suppose we are given T as a concrete adjacency matrix of 0s and 1s (or trues and
falses). The same representation will be the output G.

2 ~ Algorithm. The formula for G is a predicate implementing the recursive algorithm above. 
Notice that for the recursive part of the formula [(k v) ∈ E], we CANNOT reference E. This will
cause spurious edges. We must invoke this algo recursively. 

3 ~ Termination. To ensure termination the fuel/length of path/number of recusive calls is bounded
by the number of vertices in V.
|#

; (Listof Bool) Natural -> (Listof SymbolicBoolean)
(define (connectivity T num-vert)
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
                   (and (and (Tedge u k)
                             (Tedge k u))
                        (path? k v)))]))

  (for* ([i (in-range num-vert)]
         [j (in-range num-vert)])
    (assert (equal? (path? i j) (Gedge i j))))

  G)


(module+ test
  (require rackunit)

  ; (Listof Integer) -> (Listof bool)
  (define (int->bool m)
    (map (λ (i) (positive? i)) m))

  ; checks that the only solution to (connectivity input) is the expected matrix
  ; (Listof Integer) (Listof Integer) -> Bool
  (define-simple-check (exact-solution? input expected)
    (result-value ; extract the final value
     (with-vc (vc) ; isolate each test case
       (begin
         (define input-tree (int->bool input))
         (define num-vert (sqrt (length input-tree)))
         (define actual (connectivity input-tree num-vert))
         (define expected-bools (int->bool expected))
         ; solve for any solution
         (define M (solve (assert #t)))
         ; solve for a solution which doesn't match the expected
         (define Mnot (solve (assert (not (equal? actual
                                                  expected-bools)))))
         (cond [(and (not (unsat? M))
                     (equal? (evaluate actual M) expected-bools)
                     (unsat? Mnot))
                (displayln "ok")
                #t] 
               [else ; print out errors
                (displayln (format "input ~a" input))
                (when (unsat? M) (displayln "no solution found"))
                (unless (unsat? Mnot)
                  (define (Medge i j) (Mnot (list-ref actual (+ (* i num-vert) j))))
                  (for ([i (in-range num-vert)])
                    (for ([j (in-range num-vert)])
                      (display (if (Medge i j)
                                   1 0)))
                    (display "\n")))
                #f])))))
    
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
         0 0 1 1))) 