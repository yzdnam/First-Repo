;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |33.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x)) ; λ
(define ex2 '(λ (x) y)) ; λ
(define ex3 '(λ (y) (λ (x) y))) ; λ
(define ex4 '((λ (x) x) (λ (x) x))) ; application
(define ex5 '((λ (x) (x x)) (λ (x) (x x)))) ; application
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w))) ; application

; EX 512 - define the following:
; is-var? is-λ?, and is-app?
; λ-para, λ-body, app-fun, app-arg

; X -> Boolean
(define (is-lam? x)
  (or (is-var? x)
      (is-λ? x)
      (is-app? x)))

; X -> Boolean
(define (is-var? expr)
  (and (symbol? expr)
       (not (symbol=? 'λ expr))))

; X -> Boolean
(define (is-λ? expr)
  (and (list? expr)
       (equal? (length expr) 3)
       (symbol=? 'λ (first expr))
       (and (list? (second expr))
            (is-var? (first (second expr))))
       (is-lam? (third expr))))

; X -> Boolean
(define (is-app? expr)
  (and (list? expr)
       (equal? (length expr) 2)
       (is-lam? (first expr))
       (is-lam? (second expr))))

; Lambda-Expr -> Variable
(define (λ-para a-λ)
  (first (second a-λ)))

; Lambda-Expr -> Lam
(define (λ-body a-λ)
  (third a-λ))

; Application -> Lam
(define (app-fun an-app)
  (first an-app))

; Application -> Lam
(define (app-arg an-app)
  (second an-app))

; Lambda-Expr -> [List-of Variable]
(define (declareds a-λ)
  (cond
    [(is-λ? (λ-body a-λ)) (append (declareds (λ-body a-λ)) (list (λ-para a-λ)))]
    [else (list (λ-para a-λ))]))
(check-expect (declareds ex3) (list 'x 'y))

; EX 513 - develop a data representation for the same subset of ISL+ that uses structures instead of lists and provide data representations for ex1 ex2 and ex3 following the new data
; definition

; A Lam is one of:
; - a Symbol
; - a lambda-expr structure
; - an app-expr structure

(define-struct lambda-expr (param body))
; a lambda-expr's param is a single Symbol
; a lambda-expr's body is a Lam

(define-struct app-expr (func arg))
; an app-expr's func and arg are both Lam's

(define ex1alt (make-lambda-expr 'x 'x))
(define ex2alt (make-lambda-expr 'x 'y))
(define ex3alt (make-lambda-expr 'y (make-lambda-expr 'x 'y)))

; Lam -> Lam 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; EX 514 - make up an ISL+ expression in which x occurs both free and bound. formulate it as an element of Lam. Does undeclareds work properly on your expression? it does.
(define expr.514 (list (list 'λ (list 'x) 'x) 'x))

; EX 515 - modify undeclareds so it replaces a free occurrence of 'x with '*undeclared:x and a bound one 'y with '*declared:y
; test undeclareds on the following expression then complete the exercise:
(define expr.515 '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))

; Lam -> Lam 
(define (undeclareds.515 le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) (label-var 'declared le) (label-var '*undeclared le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Symbol Symbol -> Symbol
; combines two symbols into one a la: 'symbol1:symbol2
(define (label-var label var)
  (string->symbol (string-append (symbol->string label) ":" (symbol->string var))))
(check-expect (label-var 'fart 'face) 'fart:face)

; EX 516 - redesign undeclareds using the structure-based data representation from EX 513
; Lam -> Lam 
(define (undeclareds.516 le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) (label-var 'declared le) (label-var '*undeclared le))]
              [(lambda-expr? le)
               (local ((define para (lambda-expr-param le))
                       (define body (lambda-expr-body le))
                       (define newd (cons para declareds)))
                 (make-lambda-expr para (undeclareds/a body newd)))]
              [(app-expr? le)
               (local ((define fun (app-expr-func le))
                       (define arg (app-expr-arg le)))
                 (make-app-expr (undeclareds/a fun declareds)
                                (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

(define expr.515.struct (make-lambda-expr '*undeclared (make-app-expr (make-lambda-expr 'x (make-app-expr 'x '*undeclared)) 'y)))

; EX 517 - design static-distance which replaces all occurrences of variables with a natural number that represents how far away the declaring λ is
(check-expect (static-distances '((λ (x) ((λ (y) (y x)) x)) (λ (z) z))) '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))
(define (static-distances le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) (index le declareds) (label-var '*undeclared le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; X [List-of X] -> Number
; returns the index of the given item in the given list
(define (index x lox0)
  (local (; [List-of X] Number -> Number
          ; the accumulator is how many items in lox0 precede lox
          (define (index/a lox a)
            (cond
              [(empty? lox) (error "given item is not in list")]
              [else (if (equal? x (first lox)) a (index/a (rest lox) (add1 a)))])))
    (index/a lox0 0)))
(check-expect (index 'x (list 'y 'z 'x)) 2)