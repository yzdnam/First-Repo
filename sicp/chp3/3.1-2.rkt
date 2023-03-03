#lang sicp

; Section 3.1.1 - Local State Variables
; Examples in text:

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
; (W1 50)
; 50
; (W2 70)
; 30

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

; (define acc (make-account 100))
; ((acc 'withdraw) 50)
; 50
; ((acc 'withdraw) 60)
; "Insufficient funds"
; ((acc 'deposit) 40)
; 90
; ((acc 'withdraw) 60)
; 30

; EX 3.1
; write make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should
; specify the initial value of the sum

(define (make-accumulator amt)
  (define (accum additional)
    (set! amt (+ amt additional))
    amt)
  accum)

; (define A (make-accumulator 5))
; (A 10)
; (A 10)

; EX 3.2
; write make-monitored that takes as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure, say mf, that keeps track of the
; number of times it has been called by maintaining an internal counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter.
; if the input is the special symbol reset-count, then mf resets the counter to zero. for any other input, mf returns the result of calling f on that input and increments the counter

(define (make-monitored f)
  (let ((count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (begin
                    (set! count (inc count))
                    (f m)))))
    mf))
  

; (define s (make-monitored sqrt))
; (s 100)
; (s 'how-many-calls?)

; EX 3.3
; modify the make-account procedure so that it creates password-protected accounts. it should take a symbol as an additional argument and the resulting account object should process
; a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint
(define (make-account3.3 balance pw)
  (let ((bad-attempts 0) (max-bad-attempts 7))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (open-joint new-pw)
      (let ((j-bal balance))
        (define (joint-dispatch)
          (gen-dispatch new-pw))
        (joint-dispatch)))

    ; dispatch sub-procedure for make-account without an open-joint feature
    ;(define (dispatch in-pw m)
    ;  (if (eq? in-pw pw)
    ;      (cond ((eq? m 'withdraw) withdraw)
    ;            ((eq? m 'deposit) deposit)
    ;            ((eq? m 'open-joint) open-joint)
    ;            (else (error "Unknown request: MAKE-ACCOUNT"
    ;                         m)))
    ;      (begin (set! bad-attempts (inc bad-attempts))
    ;             (if (> bad-attempts max-bad-attempts)
    ;                 (call-the-cops)
    ;                 (error "Incorrect password")))))
    (define (gen-dispatch control-pw)
      (lambda (in-pw m)
      (if (eq? in-pw control-pw)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'open-joint) open-joint)
                (else (error "Unknown request: MAKE-ACCOUNT"
                             m)))
          (begin (set! bad-attempts (inc bad-attempts))
                 (if (> bad-attempts max-bad-attempts)
                     (call-the-cops)
                     (error "Incorrect password"))))))
        
    (gen-dispatch pw)))

(define (call-the-cops) (display "the cops have been called"))

(define (make-joint orig-acc orig-pass new-pass)
  ((orig-acc orig-pass 'open-joint) new-pass))

; original account for test
(define peter-acc (make-account3.3 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

; (define acc (make-account3.3 100 'secret-password))
; ((paul-acc 'rosebud 'withdraw) 40)
; ((acc 'some-other-password 'deposit) 50)
; ((peter-acc 'open-sesame 'withdraw) 20)

; EX 3.4
; modify the make-account procedure of ex 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password,
; it invokes the procedure call-the-cops
; see above

; Section 3.1.2 - The Benefits of Introducing Assignment

(define random-init (random 100))

;(define rand (let ((x random-init))
;               (lambda ()
;                 (set! x (rand-update x))
;                 x)))

; the procedures below takes advantage of assignment using rand instead of using rand-update directly

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random 100) (random 100)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

; the procedure below uses rand-update directly
;(define (estimate-pi-not-modular trials)
;  (sqrt (/ 6 (random-gcd-test trials random-init))))
;(define (random-gcd-test trials initial-x)
;  (define (iter trials-remaining trials-passed x)
;    (let ((x1 (rand-update x)))
;      (let ((x2 (rand-update x1)))
;        (cond ((= trials-remaining 0)
;               (/ trials-passed trials))
;              ((= (gcd x1 x2) 1)
;               (iter (- trials-remaining 1)
;                     (+ trials-passed 1)
;                     x2))
;              (else
;               (iter (- trials-remaining 1)
;                     trials-passed
;                     x2))))))
;  (iter trials 0 initial-x))
; using rand, which encapsulates the state of the random-number generator so that the details of the random-number generation remain independent of the rest of the program, allows the
; first version of the program to express the Monte Carlo method as a general procedure that takes an arbitrary experiment procedure as an argument.

; in the second version of the program, with no local state for the random-number generator, random-gcd-test must explicitly manipulate the random numbers x1 and x2. the explicit
; handling of only two numbers, the type of test being conducted, and the top level procedure having to be concerned with providing an initial random number make it difficult to
; isolate the Monte Carlo idea so that it can be applied to other tasks

; EX 3.5
; implement monte carlo integration as a procedure estimate-integral that takes as arguments a predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle and the
; number of trials to perform in order to produce the estimate. use the same monte-carlo procedure that was used above to estimate pi. use your estimate-integral to produce an
; estimate of pi by measuring the area of a unit circle
(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((boundary-area (* (- x2 x1) (- y2 y1))))
    (define (P-experiment) (P (random-in-range x1 x2) (random-in-range y1 y2)))
    (* (monte-carlo trials P-experiment) boundary-area)))

(define (unit-circle-predicate x y)
  (>= 1 (+ (square (- x 1)) (square (- y 1)))))

 (estimate-integral unit-circle-predicate 0.0 5.0 0.0 5.0 1000000)

; EX 3.6
; design a new rand procedure that, when called with the symbol 'generate, produces a new random number, and when called with the symbol 'reset, returns a procedure that sets the
; internal state variable to the given new value

; original rand procedure for reference
; (define rand (let ((x random-init))
;                (lambda ()
;                  (set! x (rand-update x))
;                  x)))

; solution. not executable because random-init and rand-update are theoretical concepts introduced by the text
;(define (rand3.6)
;  (let ((x random-init))
;    (define (dispatch m)
;      (cond ((eq? m 'generate)
;             (lambda ()
;               (set! x (rand-update x))
;               x))
;            ((eq? m 'reset)
;             (lambda (new-value)
;               (set! x new-value)
;               x))))
;    dispatch))
             
; EX 3.7
; define a make-joint procedure for the purposes of creating joint bank accounts. it should take 3 arguments. 1st is the password-protected account. 2nd is the password to the
; first argument. 3rd is a new password to be used to access the original account through the newly created joint account
; See below EX 3.3 for solution

; EX 3.8
; when we introduce assignment, the order in which arguments to a procedure are evaluated can make a difference to the result. define a simple procedure f such that evaluating
; (+ (f 0) (f 1))
; will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.
(define (ex3.8 base)
    (lambda (input)
      (if (zero? base)
          base
          (begin (set! base input) base))))

(define f (ex3.8 -1))
        
;(+ (f 0) (f 1))
;(+ (f 1) (f 0))

; EX 3.9 on paper

; EX 3.10 on paper

; EX 3.11 on paper