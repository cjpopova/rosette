#lang rosette

#| the V's should be distributed in this pattern:
0: {init-env IDs}
1: result from 0
2: result from 1
...
TOKEN: result from TOKEN-1
TOKEN+1: ∅
TOKEN+2: ∅
...
num-insts+1: {never-in-scope IDs}
|#


(define num-insts 10)
(define-symbolic TOKEN integer?)
(define-symbolic V integer? #:length 10)
(assume (= TOKEN 3))


#| setup the first 2 V's as init-env
and the rest for testing
|#
(define zeros (take V 2))
(define not-zeros (list-tail V 2))

(for ([v zeros])
  (assume (= v 0)))
(assume (and (= (first not-zeros) 1)
             (= (second not-zeros) 2)))

; ∀ V_n . 0 < V_n AND (V_n <= TOKEN OR V_n == num-insts + 1)
(for ([v not-zeros])
  (assert (and
           ; lower bound:
           (0 . < . v)
           ; upper bound:
               (or (v . <= . TOKEN)
                   (v . = . (add1 num-insts))))))

; 1 per line (but only when the line isn't 0 or \ell + 1
; note: distinct? works on values, not lists, so we need apply
(assert (apply
         distinct? 
         (filter (λ (v)
                   (not (or (= v 0)
                            (= v (add1 num-insts)))))
                 V)))


(define M (solve (assert #t)))
(displayln M)