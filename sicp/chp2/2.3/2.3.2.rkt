#lang sicp

; EX 2.56
; add a new clause to the deriv program and define appropriate procedures exponentiation?, base, exponent, and make-exponentiation
; use the symbol ** to denote exponentiation
; build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself

; EX 2.57
; extend the differentiation program to handle sums and products of arbitrary numbers of terms. do this by changing only the representation for sums and products


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e))
     (expt b e))
    (else (list '** b e))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (if (eq? (length s) 3) (caddr s) (cons '+ (cdr (cdr s)))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (if (eq? (length p) 3) (caddr p) (cons '* (cdr (cdr p)))))

(define test-eq1 '(* (* x y) (+ x 3)))
(define test-eq2 '(* x y (+ x 3)))
(multiplicand test-eq1)
(deriv test-eq1 'x)
(deriv test-eq2 'x)

; EX 2.58
; the following sub-exercises demonstrate how to modify the differentiation program by solely changing the representation of the algebraic expressions on which the
; differentiator is to operate

; EX 2.58a.
; show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y+ 2)))). To simplify the task, assume that
; + and * always take two arguments and that expressions are fully parenthesized
(define (deriv2.58a exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?2.58a exp) (make-sum2.58a (deriv2.58a (addend2.58a exp) var)
                                        (deriv2.58a (augend2.58a exp) var)))
        ((product?2.58a exp)
         (make-sum2.58a
          (make-product2.58a (multiplier2.58a exp)
                             (deriv2.58a (multiplicand2.58a exp) var))
          (make-product2.58a (deriv2.58a (multiplier2.58a exp) var)
                             (multiplicand2.58a exp))))
        ((exponentiation?2.58a exp)
         (make-product2.58a
          (make-product2.58a (exponent2.58a exp) (make-exponentiation2.58a (base2.58a exp) (- (exponent2.58a exp) 1)))
          (deriv2.58a (base2.58a exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (exponentiation?2.58a x) (and (pair? x) (eq? (cadr x) '**)))

(define (base2.58a e) (car e))

(define (exponent2.58a e) (caddr e))

(define (make-exponentiation2.58a b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e))
     (expt b e))
    (else (list b '** e))))

(define (make-sum2.58a a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product2.58a m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum?2.58a x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend2.58a s) (car s))

(define (augend2.58a s) (caddr s)) ; assuming that + always takes two arguments

(define (product?2.58a x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier2.58a p) (car p))

(define (multiplicand2.58a p) (caddr p))

(define test-eq3 '((x * y) * (x + 3)))
(define test-eq4 '(x + (3 * (x + (y + 2)))))
(deriv2.58a test-eq3 'x)
(deriv2.58a test-eq4 'x)

; EX 2.58b.
; design appropriate predicates, selectors, and constructors for standard algebraic notation ie. (x + 3 * (x + y + 2)), which drops unnecessary parentheses and
; assumes that multiplication is done before addition
(define (deriv2.58b exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?2.58b exp) (make-sum2.58b (deriv2.58b (addend2.58b exp) var)
                                        (deriv2.58b (augend2.58b exp) var)))
        ((product?2.58b exp)
         (make-sum2.58b
          (make-product2.58b (multiplier2.58b exp)
                             (deriv2.58b (multiplicand2.58b exp) var))
          (make-product2.58b (deriv2.58b (multiplier2.58b exp) var)
                             (multiplicand2.58b exp))))
;        ((exponentiation?2.58b exp)
;         (make-product2.58b
;          (make-product2.58b (exponent2.58b exp) (make-exponentiation2.58b (base2.58b exp) (- (exponent2.58b exp) 1)))
;          (deriv2.58b (base2.58b exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (make-sum2.58b a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product2.58b m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum?2.58b x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend2.58b s) (car s))

(define (augend2.58b s) (if (= (length s) 3) (caddr s) (cddr s)))

(define (product?2.58b x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier2.58b p) (car p))

(define (multiplicand2.58b p) (if (= (length p) 3) (caddr p) (cddr p)))

;(define (exponentiation?2.58b x) (and (pair? x) (eq? (cadr x) '**)))

;(define (base2.58a e) (car e))

;(define (exponent2.58a e) (caddr e))

;(define (make-exponentiation2.58a b e)
;  (cond
;    ((=number? e 0) 1)
;    ((=number? e 1) b)
;    ((and (number? b) (number? e))
;     (expt b e))
;    (else (list b '** e))))

(define test-eq5 '(x + 3 * (x + y + 2)))
(deriv2.58b test-eq5 'x)
(define test-eq6 '(x * y * (x + 3)))
(deriv2.58b test-eq6 'x)