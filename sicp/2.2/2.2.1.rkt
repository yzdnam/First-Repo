#lang sicp

(define (square n) (* n n))

; EX 2.17
; define a procedure, last-pair, which returns a list containing the last item from the given non-empty list
(define (last-pair li)
  (cond
    [(null? (cdr li)) (list (car li))]
    [else (last-pair (cdr li))]))

(last-pair (list 23 72 149 34))

; EX 2.18
; define a procedure, reverse, that takes a list as an argument and returns a lsit of the same elements in reverse order
(define (reverse li)
  (define (reverse/a li0 li/a)
    (cond
      [(null? li0) li/a]
      [else (reverse/a (cdr li0) (cons (car li0) li/a))]))
  (reverse/a li '()))

(reverse (list 1 4 9 16 25))

; EX 2.19
; define the procedures first-denomination, except-first-denomination, and no-more? for the new cc procedure
; the order of the list coin-values does not affect the answer produced by cc because each combination of coin values equalling the given amount will be iterated
; through during the execution of the procedure
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination li)
  (car li))
(define (except-first-denomination li)
  (cdr li))
(define (no-more? li)
  (null? li))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)

; EX 2.20
; use dotted tail notation to write same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first
; argument
(define (same-parity x . z)
  (define (same-parity/a li z/a)
    (cond
      [(null? z/a) (cons x (reverse li))]
      [else (if (= (remainder (car z/a) 2) (remainder x 2))
                (same-parity/a (cons (car z/a) li) (cdr z/a))
                (same-parity/a li (cdr z/a)))]))
  (same-parity/a '() z))
                
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; EX 2.21
; define two implementations of square-list which accepts a list of numbers and returns a list of the squares of those numbers
(define (square-list li)
  (if (null? li)
      nil
      (cons (* (car li) (car li)) (square-list (cdr li)))))
(define (map-sqr-list li)
  (map (lambda (x) (* x x)) li))

(square-list (list 1 2 3 4))
(map-sqr-list (list 1 2 3 4))

; EX 2.22
; thought exercise about why an iterative version of square-list doesn't work unless you use append or reverse

(define (sqr-list-try items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
(sqr-list-try (list 1 2 3 4))


; EX 2.23
; define an implementation of for-each which applies the given procedure to each of the elements in turn, from left to right and does nothing with the values returned
(define (for-each.2.23 proc li)
  (cond
    [(null? li) (newline) nil]
    [(null? (cdr li)) (proc (car li))]
    [else (proc (car li)) (for-each.2.23 proc (cdr li))]))

(for-each.2.23 (lambda (x)
                 (newline)
                 (display x))
               (list 57 321 88))