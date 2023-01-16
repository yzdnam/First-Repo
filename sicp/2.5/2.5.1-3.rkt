#lang sicp

(define (sorted? lon)
  (cond ((or (null? lon) (null? (cdr lon))) #t)
        ((> (car lon) (cadr lon)) (sorted? (cdr lon)))
        (else #f)))

; 2.5.1 Generic Arithmetic Operations
;;; foundation for section 2.5.1 until line 69

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

; complex-arithmetic selectors access the table using apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (gen-sqrt (add (mul (real-part z) (real-part z))
                   (mul (imag-part z) (imag-part z)))))
  (define (angle z)
    (gen-atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (gen-cos a)) (mul r (gen-sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (gen-cos (angle z))))
  (define (imag-part z) (mul (magnitude z) (gen-sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (gen-sqrt (add (mul x x) (mul y y)))
          (gen-atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; beginning of section 2.5.1 content
; generic arithmetic procedures:
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic2.84 'project x))
(define (gen-sine x) (apply-generic2.84 'gen-sine x))
(define (gen-cos x) (apply-generic2.84 'gen-cos x))
(define (gen-sqrt x) (apply-generic2.84 'gen-sqrt x))
(define (change-to-real x) (apply-generic2.84 'change-to-real x))
(define (gen-atan x y) (apply-generic2.84 'gen-atan (change-to-real x) (change-to-real y)))
(define (negate x) (apply-generic2.84 'negate x))
(define (gen-gcd x y) (apply-generic2.84 'gcd x y))
(define (gen-remainder x y) (apply-generic2.84 'remainder x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational (contents x) 1)))
  (put 'gen-sine '(scheme-number)
       (lambda (x) (make-real (sin x))))
  (put 'gen-cos '(scheme-number)
       (lambda (x) (make-real (cos x))))
  (put 'gen-sqrt '(scheme-number)
       (lambda (x) (make-real (sqrt x))))
  (put 'change-to-real '(scheme-number)
       (lambda (x) (make-real x)))
  (put 'negate '(scheme-number)
       (lambda (x) (tag (* x -1))))
  (put 'gcd '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'remainder '(scheme-number scheme-number)
       (lambda (x y) (tag (remainder x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gen-gcd n d)))
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ-rats? x y) ; works because make-rat uses gcd
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (rat=zero? x)
    (=zero? (numer x)))
  (define (raise x) ; this function and all that follow within this package are not currently compatible with rational functions
    (make-real (/ (numer x) (denom x))))
  (define (project x)
    (make-scheme-number (round (/ (numer x) (denom x)))))
  (define (gen-sine x)
    (make-real (sin (/ (numer x) (denom x)))))
  (define (gen-cos x)
    (make-real (cos (/ (numer x) (denom x)))))
  (define (gen-sqrt x)
    (make-real (/ (sqrt (numer x)) (sqrt (denom x)))))
  (define (negate x)
    (make-rat (mul (numer x) -1)
              (denom x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rats? x y)))
  (put '=zero? '(rational)
       (lambda (x) (rat=zero? x)))
  (put 'raise '(rational)
       (lambda (x) (raise x)))
  (put 'project '(rational)
       (lambda (x) (project x)))
  (put 'gen-sine '(rational)
       (lambda (x) (gen-sine x)))
  (put 'gen-cos '(rational)
       (lambda (x) (gen-cos x)))
  (put 'gen-sqrt '(rational)
       (lambda (x) (gen-sqrt x)))
  (put 'change-to-real '(rational)
       (lambda (x) (raise x)))
  (put 'negate '(rational)
       (lambda (x) (tag (negate x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
  
(define (make-rational n d)
  ((get 'make 'rational) n d))
  
(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (raise x)
    (make-complex-from-real-imag x 0))
  (define (project x)
    (make-rational (round x) 1))
  (define (gen-atan y x)
    (atan y x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (raise x)))
  (put 'project '(real)
       (lambda (x) (project x)))
  (put 'gen-sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'gen-cos '(real)
       (lambda (x) (tag (cos x))))
  (put 'gen-sqrt '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'gen-atan '(real real)
       (lambda (y x) (tag (gen-atan y x))))
  (put 'change-to-real '(real)
       (lambda (x) (tag x)))
  (put 'negate '(real)
       (lambda (x) (tag (* x -1))))
  (put 'gcd '(real real)
       (lambda (x y) (tag (gcd x y))))
  (put 'make 'real (lambda (x) (tag x)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  (define (project z)
    (make-real (real-part z)))
  (define (negate z)
    (make-from-real-imag (negate (real-part z)) (negate (imag-part z))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero? z)))
  (put 'project '(complex)
       (lambda (z) (project z)))
  (put 'change-to-real '(complex)
       (lambda (z) (project z)))
  (put 'negate '(complex)
       (lambda (z) (tag (negate z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

; for programs outside the complex-number package to construct complex numbers, in rectangular or polar form, the complex package procedures to make complex numbers
; in rectangular and polar form are exported from the complex package to the rest of the generic arithmetic system in the following manner:
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; EX 2.77
; (define z (make-complex-from-real-imag 3 4))
; apply-generic signals an error when the expression (magnitude z) is evaluated. this is because the complex-number selectors were never defined for complex numbers,
; just for polar and rectangular numbers. To make (magnitude z) work, the following needs to be added to the complex package:

; (put 'real-part '(complex) real-part)
; (put 'imag-part '(complex) imag-part)
; (put 'magnitude '(complex) magnitude)
; (put 'angle '(complex) angle)

; describe why this works. trace through all the procedures called in evaluating the expression (magnitude z). in particular, how many times is apply-generic invoked
; and what procedure is dispatched to in each case?

; apply-generic is invoked twice. the first time, it invokes the magnitude procedure for complex numbers which is apply-generic. the second time, it invokes the
; magnitude procedure for rectangular complex numbers.

; 1. the definition of z substitutes to ('complex 'rectangular 3 4)
; 2. (magnitude ('complex 'rectangular 3 4)) substitutes to (apply-generic 'magnitude ('complex 'rectangular 3 4))
; 3. (apply-generic 'magnitude ('complex 'rectangular 3 4)) substitutes to (apply-generic 'magnitude ('rectangular 3 4))
; 4. (apply-generic 'magnitude ('rectangular 3 4)) substitutes to (sqrt (+ (square 3) (square 4)))

; EX 2.78
; Primitive predicates such as symbol? and number? determine whether data objects have particular types. Modify the definitions of type-tag, contents, and attach-tag
; from Section 2.4.2 so that our generic system takes advantage of Scheme’s internal type system. That is to say, the system should work as before except that
; ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.
(define (attach-tag2.78 type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag2.78 datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents2.78 datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

; EX 2.79
; define a generic equality predicate equ? that tests the equality of two numbers and install it in the generic arithmetic package. This operation should work for
; ordinary numbers, rational numbers, and complex numbers
; see existing package above for solution

; EX 2.80
; define a generic predicate =zero? that tests if its argument is zero and install it in the generic arithmetic package. This operation should work for the 3 types
; in the system.
; see existing package above for solution

; 2.5.2 Combining Data of Different Types
; By coercion
; assume there are put-coercion and get-coercion procedures available
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic2.5.2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (equal? (car type-tags) (cadr type-tags)))) ; EX 2.81c solution
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic2.5.2 op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic2.5.2 op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
; By hierarchies of types

; EX. 2.81
; a hypothetical programmer reasons that apply-generic2.5.2 may try to coerce the arguments to each other's type even if they already have the same type so he puts
; procedures in the coercion table to coerce arguments of each type to their own type such as scheme-number->scheme-number or complex->complex.

; a. what happens if apply-generic2.5.2 is called with two arguments of the same type for an operation that is not found in the table for those types?
; apply-generic2.5.2 will get stuck in a loop because t1->t2 will never return false and apply-generic2.5.2 will in turn be called on the same arguments as before

; b. is the hypothetical programmer correct that something had to be done about coercion with arguments of the same type, or does apply-generic2.5.2 work correctly
; as is?
; apply-generic2.5.2 works correctly as is. if a procedure exists for (get op type-tags) then (apply proc (map contents args)) will be called.

; c. modify apply-generic2.5.2 so that it doesn't try coercion if the two arguments have the same type
; see procedure above

; EX 2.82
; show how to generalize apply-generic to handle coercion in the general case of multiple arguments. Attempt to coerce all the arguments to the type of the first
; argument, then to the type of the second argument, and so on. This is what is implemented in apply-generic 2.5.2 except only for 2 arguments.

(define (apply-generic2.82 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ((define (change-type-tags coercing-type lo-types0 lo-types counter)
             (cond
               ((null? lo-types) (apply-generic2.82 op . (map (lambda (1arg) ((get-coercion (type-tag 1arg) coercing-type) 1arg)) args)))
               ((or (get-coercion (car lo-types) coercing-type)
                    (equal? coercing-type (car lo-types))) (change-type-tags coercing-type lo-types0 (cdr lo-types) counter))
               ((= counter (length lo-types0)) (error "No method for these types" (list op type-tags)))
               (else (change-type-tags (list-ref lo-types0 counter) lo-types0 (inc counter)))))
           (change-type-tags (car type-tags) type-tags type-tags 1))
          ))))

; Give an example of where this strategy is not sufficiently general:
; when an operation for the various types of the given arguments is only defined for a supertype to those arguments for example an operation called on a triangle
; and a parallelogram. the operation would need to be called on two polygons since this is the only way the types of the two arguments could be coerced into a common
; form

; EX 2.83
; for the tower of types, integer -> rational -> real -> complex, for each type, design a procedure that raises objects of that type one level in the tower. Show
; how to install a generic raise operation that will work for each type (except complex)
; see generic arithmetic package above for solution

; EX 2.84
; use the raise operation of exercise 2.83 to modify apply-generic so that it coerces its arguments to have the same type by the method of successive raising.
; you will need to devise a way to test which of the two types is higher in the tower. do this in a manner that is "compatible" with the rest of the system and will
; not lead to problems in adding new levels to the tower

; possible ways to test which of two types is higher:
; -add numbers to type-tags to indicate location in hierarchy
; -if an argument has the same type as another argument, apply the given operation. otherwise, raise one of the arguments then check to see if it has the same type as
; the other, if not, repeat the process until the top of the hierarchy is reached. if the top of the hierarchy is reached for one argument, raise the other argument
; until it reaches the top of the hierarchy as well... INEFFICIENT if more than 2 types and first type is less than second type but greater than third type. first
; type will be raised to second type then second type will be raised to the top of the hierarchy and then both the first and the third type will need to be raised
; to that level as well.
; -rearrange args until their types are in ascending order then raise the args iteratively until all args share the same type CANNOT explicitely name arg-types when
; sorting. this will lead to problems in adding new levels to the tower
(define (apply-generic2.84 op . args)
  (define (change-types-w-raise args0 new-args)
  ; accepts the original list of arguments, returns list of arguments with all the same type
    (let ((first-arg (car args0))
         (next-arg (cadr args0))
         (first-arg-type (type-tag (car args0)))
         (next-arg-type (type-tag (cadr args0))))
      (cond
        ((null? args0) (if ((define (all-equal? lo-types)
                              (cond
                                ((null? lo-types) #t)
                                ((equal? (car lo-types) (cadr lo-types)) (all-equal? (cdr lo-types)))
                                (else #f)))
                            (all-equal? (map type-tag new-args)))
                           new-args
                           (change-types-w-raise new-args '())))
        ((equal? first-arg-type next-arg-type) (change-types-w-raise (cdr args0) (append new-args (list first-arg))))
        ((higher-type? first-arg next-arg 'complex) (change-types-w-raise (cons (raise first-arg) (cdr args0)) new-args))
        (else (change-types-w-raise (change-types-w-raise (cdr args0) (append new-args (list first-arg))))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if
       proc
       (apply proc (map contents args))
       (let ((uniform-args (change-types-w-raise args '())))
         (let ((uniform-type-tags (map type-tag uniform-args)))
           (let ((proc-for-uniform-types (get op uniform-type-tags)))
             (if
              proc-for-uniform-types
              (drop (apply proc-for-uniform-types (map contents uniform-args))) ; solution to final part of EX 2.85
              (error
               "No method for these types: APPLY-GENERIC"
               (list op uniform-type-tags))))))))))

; helper for EX 2.84
(define (in-list? symb lo-syms)
  (cond
    ((null? lo-syms) #f)
    ((equal? (car lo-syms) symb) #t)
    (else (in-list? symb (cdr lo-syms)))))
(define (higher-type? base check top-type)
  (let ((base-type (type-tag base)) (check-type (type-tag check)))
    (cond
      ((equal? base-type top-type) #f)
      ((equal? (type-tag (raise base)) check-type) #t)
      (else (higher-type? (raise base) check top-type)))))

; EX 2.85
; design a "drop" procedure that lowers a data object as far as possible in the tower of types implemented in the system above.
; plan for determining whether an object can be lowered:
; a generic operation, project, pushes an object down in the tower. a number can be dropped if, when we project it and raise the result back to the type we started
; with, we end up with something equal to what we started with
; design the various projection operations, per type, and install project as a generic operation in the system. the generic equality predicate will need to be used.
; Lastly, use drop to rewrite apply-generic2.84 so that it simplifies its answers, see line 394
(define (drop obj)
  (let ((down-one (project obj)))
    (if (equ? (raise down-one) obj)
        (drop down-one)
        obj)))

; EX 2.86
; describe and implement the changes to the system required to allow for complex numbers whose real parts, imaginary parts, magnitudes, and angles can be any type of
; number already implemented in the system

; the "make" procedures in the rectangular and polar packages are the procedures that will ultimately be called by a generic operation handling complex numbers. if
; our goal is to have these "make" procedures accept generic inputs, all of their sub-procedures must be able to accept generic inputs as well. Therefore, these
; sub-procedures must all be changed to generic procedures.

; In addition, real numbers will need to be introduced to the system because the sine and cosine of an integer or rational number will always be a real number, it
; may not remain an integer or rational number however.

; To properly add real numbers to the system, a process analogous to adding to complex numbers to the system must be followed ;;; this is actually false. it is
; optional for the purposes of the exercise but it should be implemented for logical completeness of the system since all integers and rational numbers are also
; real numbers just like all rectangular and polar numbers are complex numbers ;;; Upon further contemplation, the implementation of a structure for integers,
; rational numbers, and real numbers analogous to that of rectangular, polar, and complex numbers is redundant because the raise and drop procedures provide a means
; of efficiently translating integers and rational numbers to their real number form. Additionally, rectangular and polar numbers are complex as they are, no
; translation is necessary. On the other hand, rational numbers must be translated into their real number form making them unsuitable for the rectangular, polar,
; complex data structure template

; with the introduction of real numbers, the raise procedure for rational numbers will need to be modified to raise to a real instead of a complex number
; to implement a generic atan operation for the angle procedure on rectangular complex numbers, the atan procedure only needs to be defined for real numbers.
; for this to work, gen-atan must raise both of its arguments to real numbers before calling atan from the scheme library

; SECTION 2.5.3

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (poly=zero? poly) (empty-termlist? (term-list poly))) ; EX 2.87
  ; procedures combining polynomials
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2 (tag (sub-poly p1 p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put '=zero? '(polynomial) ; EX 2.87
       (lambda (p) (poly=zero? p)))
  (put 'negate '(polynomial)
       (lambda (p) (make-polynomial (variable p) (negate (term-list p)))))
  (put 'last-term '(polynomial)
       (lambda (p) (last-term (term-list p))))
  (put 'gcd '(polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-term order coeff) (attach-tag 'term (list order coeff))) ; needs type tag for generic adjoin-term
(define (order term) (car (contents term)))
(define (coeff term) (cadr (contents term)))
(define (the-empty-termlist) '())
(define (sort-poly-terms terms0)
  (define (count-sort-swaps terms terms/a swaps)
    (cond ((and (zero? swaps) (null? (cdr terms))) (add-to-end (car terms) terms/a))
          ((null? (cdr terms)) (count-sort-swaps (add-to-end (car terms) terms/a) (the-empty-termlist) 0))
          (else (let ((t1 (car terms)) (t2 (cadr terms)))
                  (if (>= (order t1) (order t2))
                      (count-sort-swaps (cdr terms) (add-to-end t1 terms/a) swaps)
                      (count-sort-swaps (cons t1 (cddr terms)) (add-to-end t2 terms/a) (inc swaps)))))))
    (count-sort-swaps terms0 (the-empty-termlist) 0))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; EX 2.87
; install =zero? for polynomials in the generic arithmetic package. this will allow adjoin-term to work for polynomials with coefficients that are themselves
; polynomials
; see polynomial package above 

; EX 2.88
; extend the polynomial system to include subtraction of polynomials
; see polynomial package above

; EX 2.89
; define procedures that implement the term-list representation described above as appropriate for dense polynomials
; the dense representation of "x^5 + 2x^4 + 3x^2 - 2x -5" is (1 2 0 3 -2 -5)
; the following procedures combine dense representations of polynomial term-lists. this work was redundant. a clearer way to implement both sparse and dense
; representations of polynomial term-lists is to define constructors and selectors for both representations and integrate them into generic operations on term-lists.
; this is the objective of EX 2.90.
  (define (combine-terms2.98 L1 L2 op)
    (let ((L1-highest-order (dec (length L1))) (L2-highest-order (dec (length L2))))
      (cond ((or (empty-termlist? L1) (empty-termlist? L2)) (the-empty-termlist))
            ((equal? L1-highest-order  L2-highest-order)
             (let ((t1 (first-term L1))
                   (t2 (first-term L2)))
               (adjoin-term (op t1 t2) (combine-terms2.98 (rest-terms L1) (rest-terms L2)))))
            ((> L1-highest-order L2-highest-order)
             (let ((L1s-higher-orders (reverse (list-tail (reverse L1) (length L2))))
                   (L1s-lower-orders (list-tail L1 (- L1-highest-order L2-highest-order))))
               (append L1s-higher-orders (combine-terms2.98 L1s-lower-orders L2)))) 
            ((< L1-highest-order L2-highest-order)
             (let ((L2s-higher-orders (reverse (list-tail (reverse L2) (length L1))))
                   (L2s-lower-orders (list-tail L2 (- L2-highest-order L1-highest-order))))
               (append L2s-higher-orders (combine-terms2.98 L1 L2s-lower-orders)))))))
  (define (add-terms2.98 L1 L2)
    (combine-terms2.98 L1 L2 add))
  (define (sub-terms2.98 L1 L2)
    (combine-terms2.98 L1 (negate-all-terms L2) add))
  (define (negate-all-terms2.98 loterms)
    (if (empty-termlist? loterms)
        loterms
        (let ((t1 (first-term loterms)))
          (adjoin-term (negate t1) (negate-all-terms (rest-terms loterms)))))) 
  (define (mul-terms2.98 L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (let ((L1s-1st-term-order (dec (length L1))))
          (add-terms2.98 (mul-term-by-all-terms (first-term L1) L1s-1st-term-order L2)
                         (mul-terms (rest-terms L1) L2)))))
  (define (mul-term-by-all-terms2.98 t1-coeff t1-order L0)
    (let ((new-highest-order (add t1-order (length L0))))
      (define (mul-term-by-all-terms/a L L/a orders-left) 
        (cond ((zero? orders-left) L/a)
              ((empty-termlist? L) (mul-term-by-all-terms/a L (adjoin-term 0 L/a) (dec orders-left)))
              (else (adjoin-term (mul t1-coeff (first-term L)) (mul-term-by-all-terms/a (rest-terms L)  L/a (dec orders-left))))))
      (mul-term-by-all-terms/a L0 '() new-highest-order)))

; EX 2.90
; implement a polynomial system that is efficient for both sparse and dense polynomials using a structure analogous to the complex-number example that uses both
; rectangular and polar representations
(define (install-sparse-termlist-package)
  ;; sparse representation of terms and term lists
  ;⟨procedures adjoin-term . . . coeff from text below⟩
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (let ((t1 (car term-list)))
                                   (make-term (car t1) (cadr t1))))
  (define (rest-terms term-list) (cdr term-list))
  (define (last-term NEterm-list) (let ((t-last (last-item NEterm-list)))
                                    (if (=zero? (car t-last))
                                        (make-term (car t-last) (cadr t-last))
                                        (make-term 0 0))))
  (define (empty-termlist? term-list) (equ? (the-empty-termlist) term-list))
  (define (make-sparse-termlist terms)
    (if (or (empty-termlist? terms) (sorted? (map order terms))) 
        terms
        (sort-poly-terms terms)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'sparse x)) 
  (put 'adjoin-term '(term sparse) (lambda (x y) (tag (adjoin-term x y))))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (x) (tag (rest-terms x)))) 
  (put 'last-term '(sparse) last-term)
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'make-sparse-termlist 'sparse
       (lambda (terms) (tag (make-sparse-termlist terms))))
  'done)

(define (install-dense-termlist-package)
  ; dense representation of terms and term lists
  (define (adjoin-term term term-list)
    (cond ((zero? (coeff term)) term-list)
          ((else (if (equal? (order term) (length term-list))
                     (cons (coeff term) term-list)
                     (adjoin-term term (adjoin-term (make-term (length term-list) 0) term-list)))))))
  (define (first-term term-list) (make-term (dec (length term-list)) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (last-term NEterm-list) (make-term 0 (last-item NEterm-list)))
  (define (empty-termlist? term-list) (equ? (the-empty-termlist) term-list))
  (define (make-dense-termlist terms)
    (define (construct-dense-termlist sorted-terms)
      (cond ((or (empty-termlist? sorted-terms) (empty-termlist? (rest-terms sorted-terms))) sorted-terms)
            (else (adjoin-term (first-term sorted-terms) (construct-dense-termlist (rest-terms sorted-terms))))))
    (if (or (empty-termlist? terms) (sorted? (map order terms)))
        (construct-dense-termlist terms)
        (construct-dense-termlist (sort-poly-terms terms))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'dense x))
  (put 'adjoin-term '(term dense) (lambda (x y) (tag (adjoin-term x y))))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (x) (tag (rest-terms x)))) 
  (put 'last-term '(dense) last-term)
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'make-dense-termlist 'dense
       (lambda (terms) (tag (make-dense-termlist terms))))
  'done)

(define (adjoin-term term lst) (apply-generic 'adjoin-term term lst))
(define (first-term z) (apply-generic 'first-term z))
(define (rest-terms z) (apply-generic 'rest-terms z))
(define (last-term z) (apply-generic 'last-term z))
(define (empty-termlist? z) (apply-generic 'empty-termlist? z))
                                                                                                                        
 ; TODO define install-termlist-package, analogous to complex-number-package
(define (install-termlist-package)
  ; imported procedures
  (define (make-sparse-termlist lst)
    ((get 'make-sparse-termlist 'sparse) lst))
  (define (make-dense-termlist lst)
    ((get 'make-dense-termlist 'dense) lst))
  ; procedures combining term-lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (negate-all-terms L2))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (negate-all-terms loterms)
    (if (empty-termlist? loterms)
        loterms
        (let ((t1 (first-term loterms)))
          (adjoin-term (make-term (order t1) (negate (coeff t1))) (negate-all-terms (rest-terms loterms)))))) 
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (let ((new-term (make-term new-o new-c)))
                         (let ((term-to-sub-from-dividend (mul-term-by-all-terms new-term L2)))
                           (let ((smaller-dividend (sub-terms L1 term-to-sub-from-dividend)))
                             (div-terms smaller-dividend L2))))))
                  (let ((quotient-list (car rest-of-result)) (remainder-list (cadr rest-of-result)))
                    (list (adjoin-term (make-term new-o new-c) quotient-list) remainder-list))))))))
  (define (equal-termlists? tl1 tl2)
    (cond ((and (empty-termlist? tl1) (empty-termlist? tl2)) #t)
          (else (let ((tl1-t1 (first-term tl1)) (tl2-t1 (first-term tl2)))
                  (if (and (equ? (order tl1-t1) (order tl2-t1)) (equ? (coeff tl1-t1) (coeff tl2-t1)))
                      (equal-termlists? (rest-terms tl1) (rest-terms tl2))
                      #f)))))
  (define (pseudoremainder-terms L1 L2)
    (let ((O1 (order (first-term L1))) (O2 (order (first-term L2))) (c (coeff (first-term L2))))
      (let ((i-factor (expt c (add 1 (sub O1 O2)))))
        (let ((new-L1 (mul-term-by-all-terms (make-term 0 i-factor) L1)))
          (cadr (div-terms new-L1 L2))))))
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))))
  ; interface to rest of system
  (define (tag x) (attach-tag 'termlist x))
  (put 'adjoin-term '(term termlist)
       (lambda (term lst) (tag (adjoin-term term lst))))
  (put 'first-term '(termlist) first-term)
  (put 'last-term '(termlist) last-term)
  (put 'rest-terms '(termlist)
       (lambda (terms) (tag (rest-terms terms)))) 
  (put 'empty-termlist? '(termlist) empty-termlist?)
  (put 'make-dense-termlist 'termlist
       (lambda (terms) (tag (make-dense-termlist terms))))
  (put 'make-sparse-termlist 'termlist
       (lambda (terms) (tag (make-sparse-termlist terms))))
  (put 'negate 'termlist
       (lambda (terms) (tag (negate-all-terms terms))))
  (put 'add-terms '(termlist termlist)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'sub-terms '(termlist termlist)
       (lambda (L1 L2) (tag (sub-terms L1 L2))))
  (put 'div-terms '(termlist termlist)
       (lambda (L1 L2) (tag (div-terms L1 L2))))
  (put 'mul-terms '(termlist termlist)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'equ? '(termlist termlist)
       (lambda (L1 L2) (equal-termlists? L1 L2)))
  (put 'gcd '(termlist termlist)
       (lambda (L1 L2) (tag (gcd-terms L1 L2))))
  'done)

(define (make-dense-termlist terms)
  ((get 'make-dense-termlist 'termlist) terms))
(define (make-sparse-termlist terms)
  ((get 'make-sparse-termlist 'termlist) terms))
(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
(define (sub-terms L1 L2) (apply-generic 'sub-terms L1 L2))
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
(define (div-terms L1 L2 (apply-generic 'div-terms L1 L2)))

; EX 2.91
; implement div-poly. see text for explanation of algorithm and implementation instructions
; see above termlist and polynomial packages for solution

; EX 2.92
; impose an ordering on variables to extend the polynomial package so that addition and multiplication of polynomials works for polynomials in different variables
(define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define (variable-rank var)
  (define (variable-rank-count counter)
    (if (equal? (list-ref alphabet counter) var)
        (inc counter)
        (variable-rank-count (inc counter))))
  (variable-rank-count 0))

(define (last-item lst)  (list-ref lst (dec (length lst))))

; for actual implementation, the following add-poly, mul-poly, mul-terms, and mul-term-by-all-terms need to be placed within the polynomial package.
  (define (add-poly2.92 p1 p2)
    ; the following function accepts a termlist and a polynomial. it extracts the last term of each, adds them together, adds that sum to the polynomial which is
    ; then added to the termlist
    (define (add-termlists-w-2-vars dom-tl subord-poly)
      (let ((subord-poly-var (variable subord-poly)) (subord-poly-lt (last-term subord-poly)) (dom-tl-lt (last-term dom-tl)))
        (let ((new-last-term
               (make-term 0 (add (coeff (last-term dom-tl)) (coeff subord-poly-lt))))
              (subord-poly-wo-last-term
               (sub subord-poly (make-poly subord-poly-var (make-sparse-termlist (list subord-poly-lt)))))
              (dom-tl-wo-last-term
               (sub-terms dom-tl (make-sparse-termlist (list dom-tl-lt)))))
          (let ((subord-poly-w-new-last-term
                 (add subord-poly-wo-last-term (make-poly subord-poly-var (make-sparse-termlist (list new-last-term))))))
          (add-terms dom-tl-wo-last-term (make-sparse-termlist (list (make-term 0 subord-poly-w-new-last-term))))))))
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (add-terms (term-list p1) (term-list p2))))
          ((< (variable-rank (variable p1)) (variable-rank (variable p2)))
           (make-poly (variable p1)
                      (add-termlists-w-2-vars (term-list p1) p2)))
          ((> (variable-rank (variable p1)) (variable-rank (variable p2)))
           (make-poly (variable p2)
                      (add-termlists-w-2-vars (term-list p2) p1)))
          (else (error "Invalid variable used. Only use single lowercase letters as variables."))))

  (define (mul-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (mul-terms (term-list p1) (term-list p2))))
          ((< (variable-rank (variable p1)) (variable-rank (variable p2)))
           (mul-polys-w-2-vars p1 p2))
          ((> (variable-rank (variable p1)) (variable-rank (variable p2)))
           (mul-polys-w-2-vars p2 p1))
          (else (error "Invalid variable used. Only use single lowercase letters as variables."))))

  (define (mul-polys-w-2-vars dom-poly subord-poly) 
    (if (empty-termlist? (term-list dom-poly))
        dom-poly
        (let ((dom-var (variable dom-poly)) )
          (add-poly (make-poly dom-var (mul-term-by-poly (first-term (term-list dom-poly)) subord-poly))
                    (mul-polys-w-2-vars (make-poly dom-var (rest-terms (term-list dom-poly))) subord-poly))))) 
  (define (mul-term-by-poly in-term poly)
    (if (empty-termlist? (term-list poly))
        (make-sparse-termlist (make-term (order in-term) 0))
        (let ((poly*in-term-coeff (make-poly (variable poly) (mul-term-by-all-terms (make-term 0 (coeff in-term)) (term-list poly)))))
          (make-sparse-termlist (order in-term) poly*in-term-coeff))))

; EX 2.93
; modify the rational-arithmetic package to use generic operations
; see above

; EX 2.94
; define gcd terms and its supporting function, remainder-terms then write gcd-poly and implement gen-gcd as a generic operation which calls gcd-poly on polynomials
; and gcd on real numbers and integers

; EX 2.95
(define P1 (make-poly 'x (make-sparse-termlist (make-term 2 1) (make-term 1 -2) (make-term 0 1))))
(define P2 (make-poly 'x (make-sparse-termlist (make-term 2 11) (make-term 0 7))))
(define P3 (make-poly 'x (make-sparse-termlist (make-term 1 13) (make-term 0 5))))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(define Q1Q2-gcd (gen-gcd Q1 Q2))
; Q1Q2-gcd will not be the same as P1. In this case, noninteger operations during computation cause difficulties with the GCD algorithm. Investigate and understand
; by following the computation by hand.
(define Q1-written-out (make-poly 'x (make-sparse-termlist (make-term 4 11) (make-term 3 -22) (make-term 2 18) (make-term 1 -14) (make-term 0 7))))
(define Q2-written-out (make-poly 'x (make-sparse-termlist (make-term 3 13) (make-term 2 -21) (make-term 1 13) (make-term 0 -5))))
; the issue becomes apparent when div-terms creates the first term of the quotient's coefficient by dividing the coefficients of the first terms of the dividend and
; divisor. this causes the first term of the quotient's coefficient to be 11/13 which causes the new dividend to have rational coefficients which ultimately leads to
; the actual result being different from the expected result of P1

; EX 2.96
; a) implement pseudoremainder-terms which is just like remainder-terms except that it multiplies the dividend by the integerizing factor before calling div-terms
; modify gcd-terms to use pseudoremainder-terms.
; b) modify gcd-terms so that it removes common factors from the coefficients of the answer by dividing all the coefficients by their gcd
; see above