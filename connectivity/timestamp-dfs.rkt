#lang rosette

(define num-vert 5)

(define vertices (range num-vert)) ; listof vertex indixes

(define tree-edge-list
  `((0 1) ; 0 -> 1 -> 2
    (1 2) ;        -> 3 -> 4
    (1 3)
    #;(3 4)))

; parser to check if x,y are in the tree edge list
; (should be it be bidirectional?)
(define (lookup-tree-edge x y)
  (ormap
   (λ (e) (or (and (= x (first e)) (= y (second e)))
              (and (= y (first e)) (= x (second e)))))
   tree-edge-list))


(define-symbolic* membership boolean? #:length num-vert)
(define-symbolic* timestamp integer? #:length num-vert)
(define jprimes (make-hash))

; : Integer -> Void
(define (connected? start-vertex)
  (define vert-set (list->set vertices))

  ; start vertex is a meber of the CC; its timestamp is 1
  (assert (and (list-ref membership start-vertex)
               (= 1 (list-ref timestamp start-vertex))))

  (assert (apply distinct? timestamp)) ; this might be necessary for branching

  (displayln (set-subtract vert-set (set start-vertex)))
  
  (for ([j (set-subtract vert-set (set start-vertex))]) ; for every vertex other than start,
    (define-symbolic* jprime integer?)
    (hash-set! jprimes j jprime)
    (assert
     (equal?
      (list-ref membership j) ; it is a member of the CC iff
      (exists (list jprime)
              (and
               (not (= jprime j))
               (list-ref membership jprime)
               (lookup-tree-edge jprime j)
               ((list-ref timestamp jprime) . < . (list-ref timestamp j))))))))

(connected? 0)

(define M
  (solve (assert (and #;(list-ref membership 1)
                      (list-ref membership 2)))))
(displayln M)


; why does the printing produce the correct results,
; but the actual model doesn't??
(for ([j (in-range 1 5)])
  (define jprime (hash-ref jprimes j))
  (printf "~a ~a ~a\n"
          j
          (evaluate jprime M)
          (evaluate
           (and
            (not (= jprime j))
            (list-ref membership jprime)
            (lookup-tree-edge jprime j)
            ((list-ref timestamp jprime) . < . (list-ref timestamp j)))
           M)))


#|
from papers: for every vertex j, XOR:
- j is not a member of the CC
- j is the start vertex
- ∃ j' s.t. (j j') ∈ tree-edges and TS_j' < TS_j
|#
