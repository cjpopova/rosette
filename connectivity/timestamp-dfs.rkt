#lang rosette

(define num-vert 5)

(define vertices (range num-vert))

(define tree-edge-list
  `((0 1)
    (1 2)
    (1 3)
    (3 4)))

(define (lookup-tree-edge x y)
  (ormap (λ (e) (and (= x (first e)) (= y (second e)))) tree-edge-list))


(define-symbolic* membership boolean? #:length num-vert)
(define-symbolic* timestamp integer? #:length num-vert)


(define (connected? start-vertex)
  (define vert-set (list->set vertices))

  (assume (and (= 1 (list-ref timestamp start-vertex))
               (list-ref membership start-vertex)))

  (assert (apply distinct? timestamp))

  (for ([j (set-subtract vert-set (set start-vertex))])
    (define-symbolic* jprime integer?)
    (assert
     (equal?
      (list-ref membership j)
      (exists (list jprime)
              (and
               (not (= jprime j))
               (lookup-tree-edge jprime j)
               ((list-ref timestamp jprime) . < . (list-ref timestamp j))))))))

(connected? 0)

(solve (assert #t #;(not (list-ref membership 4))))
