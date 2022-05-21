#lang rosette/safe

; Rosette guide chp 2: essentials
; 2.3.1: verfication

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))
; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; the procedure we are testing
(define (bvmid lo hi)  ; (lo + hi) / 2
  (bvsdiv (bvadd lo hi) (int32 2)))

; a group of assumptions + assertions on bvmid behavior
; (program is incorrect if an input satisfies its assumptions but
; the output fails any assertions)
(define (check-mid impl lo hi)     ; Assuming that
  (assume (bvsle (int32 0) lo))    ; 0 ≤ lo and
  (assume (bvsle lo hi))           ; lo ≤ hi,
  (define mi (impl lo hi))         ; and letting mi = impl(lo, hi) and
  (define diff                     ; diff = (hi - mi) - (mi - lo),
    (bvsub (bvsub hi mi)
           (bvsub mi lo)))         ; we require that
  (assert (bvsle lo mi))           ; lo ≤ mi,
  (assert (bvsle mi hi))           ; mi ≤ hi,
  (assert (bvsle (int32 0) diff))  ; 0 ≤ diff, and
  (assert (bvsle diff (int32 1)))) ; diff ≤ 1.

; symbolic vars for the model
(define-symbolic l h int32?)

; the model
(define cex (verify (check-mid bvmid l h)))

; 2.3.3: Angelic execution