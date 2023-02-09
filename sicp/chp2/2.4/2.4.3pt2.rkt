#lang sicp

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))
(define (sum? exp) (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp) (and (pair? exp) (eq? (car exp) '*)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier s) (cadr s))
(define (multiplicand s) (caddr s))

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

; EX 2.73

; a. explain what was done to go from deriv.v1 to deriv and why the predicates number? and variable? can't be assimilated into
; the data-directed dispatch
(define (deriv.v1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv.v1 (addend exp) var)
                   (deriv.v1 (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv.v1 (multiplicand exp) var))
                   (make-product
                    (deriv.v1 (multiplier exp) var)
                    (multiplicand exp))))
        ;⟨more rules can be added here⟩
        (else (error "unknown expression type: DERIV" exp))))

; deriv is commented out because the get procedure has not been defined yet
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; the procedure deriv and the data types sum and product have been added to an operation-and-type-table. if the expression given
; to deriv is not a number or a variable, deriv retrieves the proper procedure from the table using the operator of the
; expression to determine the expression's type. the predicates number? and variable? cannot be assimilated into the data-
; directed dispatch because the dispatch uses the expression's operator, which is also its car, as its type tag. number? and
; variable? are required to operate on an expression that is not a list and therefore has no car, in this case being referred to
; as an operator. since number? and variable?'s inputs may not have an operator, they cannot be assimilated into the data-
; directed dispatch since the dispatch uses the input's operator as a type tag.

; EX 2.73b.
; write the procedures for the derivatives of sums and products, and the auxiliary code required to install them in the table
; used by the program above
(define (install-sum-deriv-package)
  (define (sum-deriv exp var)
    (if (null? (cddr exp))
        (make-sum (deriv (car exp) var) ; two operands
                  (deriv (cadr exp) var))
        (make-sum (deriv (car exp) var) ; >two operands
                  (sum-deriv (cdr exp) var))))
  (put 'deriv '+
       (lambda (x y) (sum-deriv x y)))
  'done)

(define (install-product-deriv-package)
  (define (product-deriv exp var)
    (if (null? (cddr exp))
        (make-sum (make-product ; two operands
                   (car exp)
                   (deriv (cadr exp) var))
                  (make-product
                   (deriv (car exp) var)
                   (cadr exp)))
        (make-sum (make-product ; >two operands
                   (car exp)
                   (product-deriv (cdr exp) var))
                  (make-product
                   (deriv (car exp) var)
                   (cons '* (cdr exp))))))
  (put 'deriv '*
       (lambda (x y) (product-deriv x y)))
  'done)

; EX 2.73c.
; install the exponent differentiation rule in this data-directed system

(define (base e) (car e))

(define (exponent e) (cadr e))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e))
     (expt b e))
    (else (list '** b e))))

(define (install-exponent-deriv-package)
  (define (exponent-deriv exp var)
    (make-product
     (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
     (deriv (base exp) var)))
  (put 'deriv '**
       (lambda (x y) (exponent-deriv x y)))
  'done)

; EX 2.73d.
; explain what changes would need to be made to the algebraic manipulator system defined in the preceding exercises if the
; procedures were indexed so that, within the operation-type table, the operation to be differentiated was indexed as the
; operation and differentiation was indexed as the type

; the two given symbols in every get and put operation in the system would need to be changed around