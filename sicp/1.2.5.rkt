#lang sicp

; EX 1.20 - illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations performed using normal-order evaluation and applicative-
; order evaluation

; Normal-Order Evaluation - remainder evaluated 14 times
; (gcd 206 40)
;   (if (= 40 0) -> #f
;   206
;   (gcd 40 (remainder 206 40))))
;     (if (= (remainder 206 40) 0) -> #f
;         40
;         (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
;           (if (= (remainder 40 (remainder 206 40))) -> #f
;               (remainder 206 40)
;               (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;                 (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) -> #f
;                     (remainder 40 (remainder 206 40))
;                     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))
;                                                                                                      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; ***                   (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) -> #t
;                           (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                           (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;                                (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))
;                                                                                                                       (remainder (remainder 206 40 (remainder 40 (remainder 206 40)))))))

; Evaluated remainder operations for Normal-Order Evaluation of (gcd 206 40)
; ***                   (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) -> #t
; -> (remainder (remainder 40 6) (remainder 6 (remainder 40 6))) - Evaluated 3 times
; -> (remainder 4 (remainder 6 4)) - Evaluated 2 times
; -> (remainder 4 2) - Evaluted 1 time
; -> 0 - Evaluated 1 time
; plus 7 times when "if" had a false result
; remainder evaluated a total of 14 times for a Normal Order Evaluation of (gcd 206 40)


; Applicative-Order Evaluation - remainder evaluated 4 times
; (gcd 206 40)
;   (if (= 40 0) -> #f
;   206
;   (gcd 40 (remainder 206 40))))
;   (gcd 40 6)
;     (if (= 6 0) -> #f
;     40
;     (gcd 6 (remainder 40 6)))
;     (gcd 6 4)
;       (if (= 4 0) -> #f
;       6
;       (gcd 4 (remainder 6 4)))
;       (gcd 4 2)
;         (if (= 2 0) -> #f
;         4
;         (gcd 2 (remainder 4 2)))
;         (gcd 2 0)
;           (if (= 0 0) -> #t
;           2
;           (gcd 0 (remainder 2 0)))