#lang sicp

; EX 1.34
; with the procedure (define (f g) (g 2)), what happens if we ask the interpreter to evaluate (f f)?
; (f f) -> (f 2) -> (2 2) error: "expected a procedure that can be applied to arguments, given: 2"
(define (f g) (g 2))