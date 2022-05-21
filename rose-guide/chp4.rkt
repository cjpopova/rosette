#lang rosette/safe

(define-symbolic a b c d integer?)
(define-symbolic e f g h boolean?)
(define-symbolic x y integer?)
#|
4.2 bools, ints
|| disjunction and && conjuction do NOT have short circuiting

rosette/safe does not have error? don't if unsafe has it

# forall + exists
why does the body of forall or exists return a boolean?
recall that the side-effect assertions can come from type assertions

> (define out (with-vc (= (+ (if e x 'x) 1) y)))
> out
(normal (= y (+ 1 x)) (vc #t e))
; means that the result is (= y (+ 1 x)) provided the input is legal
(that e = #t)
(result value state)
normal is a kind of result. with-vc returns the result of symbolic evaluation
|#

#|
4.4 Uninterpreted functions
"""
in Rosette, functions are special kinds of procedures that are pure (have no side effects)
and total (defined on every input value)
"""
their type is ~> and does not permit higher order ~>

trivially true assertions (eg types which are already defined) are not added to the vc
|#