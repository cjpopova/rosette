#lang rosette
(define (≤ a b) (<= a b)) ; styling

(define-symbolic TOKEN integer?)
(assert (and (TOKEN . > . 0)
             (TOKEN . ≤ . 3))) ; we will make a program w/ num-insts=3 passes (0,1,2)


; g_j = AFTER line j, there is something in gamma that u can use
(define-symbolic
  u0 u1 u2
  g0 g1 
  boolean?)

; initialization
(assume (and u0 g0))

; previous availability (scope rules)
(assert (implies u1 g0))
(assert (implies u2 g1))

; running out @ g_j => need to stop BY (j+1)
; don't need these bc of previous availability
#|(assert (implies (not g0) (TOKEN . <= . 1)))
(assert (implies (not g1) (TOKEN . <= . 2)))|#
; did not compute g2 because whether or not it is true, TOKEN ≤ 3

; everything < token should specified ...
; and everything >= token should not ... 
(assert (implies (= TOKEN 1)
                 (and u0
                      (not u1)
                      (not u2))))
(assert (implies (= TOKEN 2)
                 (and u0
                      u1
                      (not u2))))
(assert (implies (= TOKEN 3)
                 (and u0 u1 u2)))

#| 

Nonspecification after token:
∀ U_jf, P_jqn s.t. j >= TOKEN . ¬U_jf ∧ ¬P_jqn

Uninitialized variables:
∀ V_n . V_n <= TOKEN OR V_n == num-insts + 1

^ that rule in combo w/ the exactly-1-U and exactly-1-V should force
all uninit'd variables `n` to have V_n == num-insts + 1
|#


; testing
(solve (assert (and (not g1) (= TOKEN 1))))