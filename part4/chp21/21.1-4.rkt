;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21.1-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; LESSON LEARNED: Do not make a new function for every refinement. Build on the already existing functions as new functionality is added to the project.
;;; SECTION 21.1 ;;;
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; EX 346
; a representation of a BSL Expression can evaluate to a Number

; EX 347
; [Representation of a BSL-Expr] -> Number
; Evaluates a representation of a BSL-Expr
(define (eval-expression expr)
  (local (
          (define (eval-add-mul-structure sname-left sname-right s-op)
            (cond
                   [(and (number? (sname-left expr))
                         (number? (sname-right expr))) (s-op (sname-left expr) (sname-right expr))]
                   [(and (number? (sname-left expr))
                         (or (add? (sname-right expr)) (mul? (sname-right expr))))
                    (s-op (sname-left expr) (eval-expression (sname-right expr)))]
                   [(and (or (add? (sname-left expr)) (mul? (sname-left expr)))
                         (number? (sname-right expr))) (s-op (eval-expression (sname-left expr)) (sname-right expr))]
                   [(and (or (add? (sname-left expr)) (mul? (sname-left expr)))
                         (or (add? (sname-right expr)) (mul? (sname-right expr))))
                         (s-op (eval-expression (sname-left expr)) (eval-expression (sname-right expr)))])))
    (cond
      [(number? expr) expr]
      [(add? expr) (eval-add-mul-structure add-left add-right +)]
      [(mul? expr) (eval-add-mul-structure mul-left mul-right *)])))

(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)	

; EX 348
; data representation for Boolean BSL expressions constructed from #true, #false, and, or, and not.
; A Boolean BSL Expression is one of the following:
; -#true
; -#false
; -logical evaluator
; A logical evaluator is one of the following
; -and (accepts a list of Boolean BSL Expressions)
; -or (accepts a list of Boolean BSL Expressions)
; -not (accepts 1 Boolean BSL Expression)
(define-struct and. [list-of-statements])
(define-struct or. [list-of-statements])
(define-struct not. [statement])

; [Representation of a Bool-BSL-Expr] -> Boolean
; Evaluates a representation of a Boolean-BSL-Expr
(define (eval-bool-expression expr)
  (cond
    [(boolean? expr) expr]
    [(and.? expr) (andmap eval-bool-expression (and.-list-of-statements expr))]
    [(or.? expr) (ormap eval-bool-expression (or.-list-of-statements expr))]
    [(not.? expr) (not (eval-bool-expression (not.-statement expr)))]))

;;;
(define (atom? item)
  (or (number? item)
      (string? item)
      (symbol? item)))

(define WRONG "input not recognizable as a BSL-expr")

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; EX 349
(check-expect (parse '(+ (* 3 3) (* 4 4))) (make-add (make-mul 3 3) (make-mul 4 4)))

; EX 350
; The unusual thing about the parse function's design in regards to the design recipe
; is that it includes a boolean-producing function which provides a test of whether the
; input is recognizable as a BSL-expr

; EX 351
; S-expr -> Number
; produces the value of the S-expr if it is recognizable as a BSL-expr
; otherwise, it produces the same error as parse
(define (interpreter-expr s-expr)
  (eval-expression (parse s-expr)))

(check-expect (interpreter-expr '(+ (* 3 3) (* 4 4))) 25)

;;; SECTION 21.2 ;;;
; EX 352
; BSL-var-expr Symbol Number -> BSL-var-expr
; produces the given BSL-var-expr with all occurrences of the given symbol replaced by the
; given number
(define (subst ex symb n)
  (cond
    [(number? ex) ex]
    [(equal? ex symb) n]
    [(add? ex) (make-add (subst (add-left ex) symb n) (subst (add-right ex) symb n))]
    [(mul? ex) (make-mul (subst (mul-left ex) symb n) (subst (mul-right ex) symb n))]
    [else ex]))

(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 4) (make-mul 1/2 (make-mul 4 3)))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'y 4) (make-mul 1/2 (make-mul 'x 3)))

; EX 353
; BSL-var-expr -> Boolean
; Determines whether a given BSL-var-expr has any symbols in it
(define (numeric? expr)
  (cond
    [(number? expr) #true]
    [(symbol? expr) #false]
    [(add? expr) (and (numeric? (add-left expr)) (numeric? (add-right expr)))]
    [(mul? expr) (and (numeric? (mul-left expr)) (numeric? (mul-right expr)))]))

(check-expect (numeric? (make-mul 1/2 (make-mul 'x 3))) #false)
(check-expect (numeric? (make-mul 1/2 (make-mul 4 3))) #true)

; EX 354
; BSL-var-expr -> Number
; evaluates a BSL-var-expr if it does not contain any symbols
(define (eval-variable expr)
  (cond
    [(numeric? expr) (eval-expression expr)]
    [else (error expr " contains an undefined variable")]))

(check-expect (eval-variable (make-mul 1/2 (make-mul 4 3))) 6)
(check-error (eval-variable (make-mul 1/2 (make-mul 'x 3))) "(make-mul 0.5 (make-mul 'x 3)) contains an undefined variable")

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-var-expr AL ->
; applies the substitute to the BSL-var-expr using the associations in the given AL and then
; returns the value of the expression if no variables remain inside of it or an error if variables still remain
(define (eval-variable* expr loa)
  (local (
          (define (apply-al expr loa)
            (cond
              [(empty? loa) expr]
              [else (apply-al (subst expr (first (first loa)) (second (first loa))) (rest loa))])))
    (eval-variable (apply-al expr loa))))

(check-expect (eval-variable* (make-add (make-add 'x 'y) (make-add (make-add 'z 4) 3))
                              (list (list 'x 2) (list 'y 1) (list 'z 4))) 14)

; BSL-var-expr AL -> Number
(define (eval-var-lookup e da)
  (local (
          (define (subs-with-list e)
            (cond
              [(number? e) e]
              [(symbol? e) (cond
                             [(not (boolean? (assq e da))) (subst e (first (assq e da)) (second (assq e da)))]
                             [else (error "the variable " e " is not defined")])]
              [(add? e) (make-add (eval-var-lookup (add-left e) da) (eval-var-lookup (add-right e) da))]
              [(mul? e) (make-mul (eval-var-lookup (mul-left e) da) (eval-var-lookup (mul-right e) da))])))
          (eval-variable (subs-with-list e))))
                         
(check-expect (eval-var-lookup (make-add (make-add 'x 'y) (make-add (make-add 'z 4) 3))
                              (list (list 'x 2) (list 'y 1) (list 'z 4))) 14)

(check-error (eval-var-lookup (make-add (make-add 'x 'y) (make-add (make-add 'a 4) 3))
                              (list (list 'x 2) (list 'y 1) (list 'z 4))) "the variable 'a is not defined")

;;; SECTION 21.3 ;;;
; EX 356
; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-func-app Symbol BSL-fun-expr)

(define-struct func-app [name argument])
; a func-app-name is a symbol, a func-app-argument is a BSL-fun-expr

; EX 358
; A BSL-fun-def is:
; - (make-func-def Symbol Symbol BSL-fun-expr)

; A BSL-fun-def* is one of:
; -'()
; -(cons BSL-fun-def BSL-fun-def*)

(define-struct func-def [name parameter body])

(define da-fgh (list (make-func-def 'f 'x (make-add 3 'x))
                     (make-func-def 'g 'y (make-func-app 'f (make-mul 2 'y)))
                     (make-func-def 'h 'v (make-add (make-func-app 'f 'v) (make-func-app 'g 'v)))))

; EX 357
; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; returns the value of the first given function expression, the first given symbol is a function's name
; the second symbol is the function's parameter, the second expression is the function's body
; CANNOT handle nested function applications, needs the ability to manipulate a BSL-fun-def* which is provided in
; eval-function* below
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(or
      (symbol? ex)
      (add? ex)
      (mul? ex)) (eval-variable ex)]
    [(func-app? ex) (cond
                      [(equal? (func-app-name ex) f) (local (
                                                     (define arg (func-app-argument ex))
                                                     (define value (eval-definition1 arg f x b))
                                                     (define plugd (subst b x value)))
                                               (eval-definition1 plugd f x b))]
                      [else (error (func-app-name ex) " is undefined")])]))

(check-expect (eval-definition1 (make-func-app 'f 4) 'f 'x (make-add 'x (make-mul 3 'x))) 16)
(check-expect (eval-definition1 (make-func-app 'f (make-mul 4 2)) 'f 'x (make-add 'x (make-mul 3 'x))) 32)
;(check-expect (eval-definition1 (make-func-app 'g 1) 'g 'y (make-func-app 'f (make-mul 2 'y))) 5)
;(check-expect (eval-definition1 (make-func-app 'h (make-func-app 'g 1)) 'h 'v (make-add (make-func-app 'f 'v)
;                                                                                        (make-func-app 'g 'v))) 21)

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) (make-func-def 'g 'y (make-func-app 'f (make-mul 2 'y))))
(define (lookup-def da f)
  (cond
    [(empty? da) (error f " is undefined")]
    [(equal? (func-def-name (first da)) f) (first da)]
    [else (lookup-def (rest da) f)]))

; EX 359
; BSL-fun-expr BSL-fun-def* -> Number
; produces the result that DrRacket shows if you evaluate ex in the interactions area,
; assuming the definitions area contains da.
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error ex " is undefined")]
    [(add? ex) (eval-variable (make-add (eval-function* (add-left ex) da) (eval-function* (add-right ex) da)))]
    [(mul? ex) (eval-variable (make-mul (eval-function* (mul-left ex) da) (eval-function* (mul-right ex) da)))]
    [(func-app? ex) (cond
                      [(or (and (add? (func-def-body (lookup-def da (func-app-name ex))))
                                (or (func-app? (add-left (func-def-body (lookup-def da (func-app-name ex)))))
                                    (func-app? (add-right (func-def-body (lookup-def da (func-app-name ex)))))))
                           (and (mul? (func-def-body (lookup-def da (func-app-name ex))))
                                (or (func-app? (mul-left (func-def-body (lookup-def da (func-app-name ex)))))
                                    (func-app? (mul-right (func-def-body (lookup-def da (func-app-name ex))))))))
                       (local (; add-or-muli-string -> Number
                               (define (solve-func-w-func-nested-in-add-or-mul-in-def-body opr)
                                 (local (
                                         (define definition (lookup-def da (func-app-name ex)))
                                         (define definition-body (func-def-body (lookup-def da (func-app-name ex))))
                                         (define resolved-arg (eval-function* (func-app-argument ex) da))
                                         (define func-def-opr-left
                                           (cond
                                             [(equal? opr "add") (add-left definition-body)]
                                             [(equal? opr "mul") (mul-left definition-body)]))
                                         (define func-def-opr-right
                                           (cond
                                             [(equal? opr "add") (add-right definition-body)]
                                             [(equal? opr "mul") (mul-right definition-body)]))
                                         (define evaluated-def-opr-left
                                           (cond
                                             [(func-app? func-def-opr-left)
                                                (eval-function* (make-func-app (func-app-name func-def-opr-left) resolved-arg) da)]
                                             [else (eval-function* func-def-opr-left)]))
                                         (define evaluated-def-opr-right
                                           (cond
                                             [(func-app? func-def-opr-right)
                                              (eval-function* (make-func-app (func-app-name func-def-opr-right) resolved-arg) da)]
                                             [else (eval-function* func-def-opr-right)]))
                                         (define evaluated-def-body
                                           (cond
                                             [(equal? opr "add") (make-add evaluated-def-opr-left evaluated-def-opr-right)]
                                             [(equal? opr "mul") (make-mul evaluated-def-opr-left evaluated-def-opr-right)]))
                                         (define orig-func-w-def-body-evaluated
                                         (eval-definition1 (make-func-app (func-def-name definition) resolved-arg)
                                                                          (func-def-name definition)
                                                                          (func-def-parameter definition)
                                                                          evaluated-def-body)))
                                   orig-func-w-def-body-evaluated)))
                         (cond
                           [(add? (func-def-body (lookup-def da (func-app-name ex))))
                            (solve-func-w-func-nested-in-add-or-mul-in-def-body "add")]
                           [(mul? (func-def-body (lookup-def da (func-app-name ex))))
                            (solve-func-w-func-nested-in-add-or-mul-in-def-body "mul")]))]

                      [(func-app? (func-def-body (lookup-def da (func-app-name ex))))
                         (local (
                                 (define definition (lookup-def da (func-app-name ex)))
                                 (define nested-func-app-name (func-app-name (func-def-body definition)))
                                 (define nested-func-app-arg (func-app-argument
                                                              (func-def-body definition)))
                                 (define resolved-arg (eval-function* (subst nested-func-app-arg
                                                                               (func-def-parameter definition)
                                                                               (func-app-argument ex)) da)))
                           (eval-function* (make-func-app nested-func-app-name resolved-arg) da))]
                      [(func-def? (lookup-def da (func-app-name ex)))
                         (local (
                                 (define definition (lookup-def da (func-app-name ex)))
                                 (define resolved-arg (eval-function* (func-app-argument ex) da))
                                 (define ex-w-func-applied (eval-definition1 (make-func-app (func-def-name definition) resolved-arg)
                                                                             (func-def-name definition)
                                                                             (func-def-parameter definition)
                                                                             (func-def-body definition))))
                           ex-w-func-applied)]
                      [else (error (func-app-name ex) " is undefined")])]))

(check-expect (eval-function* (make-func-app 'f 3) da-fgh) 6)
(check-expect (eval-function* (make-func-app 'g 1) da-fgh) 5)
(check-expect (eval-function* (make-func-app 'h (make-func-app 'g 1)) da-fgh) 21)

; EX 360
; a BSL-da-all is one of the following:
; -'()
; -Association List
; -BSL-fun-def*
; -[List-of BSL-da-all]

; A BSL-fun-def is:
; - (make-func-def Symbol Symbol BSL-fun-expr)

; A BSL-fun-def* is one of:
; -'()
; -(cons BSL-fun-def BSL-fun-def*)

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define EXAMPLE-DA-ALL (list
                        (list 'close-to-pi 3.14)

                        (make-func-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r)))

                        (make-func-def 'volume-of-10-cylinder 'r (make-mul 10 (make-func-app 'area-of-circle 'r)))))


; BSL-da-all Symbol -> [Maybe-Association]
; consumes a BSL-da-all, da, and a symbol, x. Produces a representation of a constant definition whose name is x
; if one exists in da. Otherwise, it produces an error informing the user that no such constant definition can be found
(define (lookup-con-def da x)
  (cond
    [(ormap list? da) (local (
                              (define (find-con-def da)
                                (cond
                                  [(empty? da) (error "the constant definition " x " cannot be found.")]
                                  [else (cond
                                          [(and (list? (first da))
                                                (equal? (first (first da)) x)) (first da)]
                                          [else (find-con-def (rest da))])])))
                        (find-con-def da))]
    [else (error "the constant definition " x " cannot be found.")]))

(check-expect (lookup-con-def EXAMPLE-DA-ALL 'close-to-pi) (list 'close-to-pi 3.14))
(check-error (lookup-con-def EXAMPLE-DA-ALL 'fartsack) "the constant definition 'fartsack cannot be found.")

; BSL-da-all Symbol -> [Maybe-BSL-func-def]
; same functionality as lookup-con-def except for function definfitions
(define (lookup-func-def da f)
  (cond
    [(ormap func-def? da) (local (
                              (define (find-func-def da)
                                (cond
                                  [(empty? da) (error "the function definition " f " cannot be found.")]
                                  [else (cond
                                          [(and (func-def? (first da))
                                                (equal? (func-def-name (first da)) f)) (first da)]
                                          [else (find-func-def (rest da))])])))
                        (find-func-def da))]
    [else (error "the function definition " f " cannot be found.")]))

(check-expect (lookup-func-def EXAMPLE-DA-ALL 'area-of-circle)
              (make-func-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))

; EX 361
; BSL-func-expr BSL-da-all -> BSL-func-expr
; produces the same value that DrRacket shows if the expression is entered at the prompt in the interactions area
; and the definitions area contains the appropriate definitions
(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (second (lookup-con-def da ex))]
    [(add? ex) (eval-function* (make-add (eval-all (add-left ex) da) (eval-all (add-right ex) da)))]
    [(mul? ex) (eval-function* (make-mul (eval-all (mul-left ex) da) (eval-all (mul-right ex) da)))]
    [(func-app? ex) (local (
                            (define definition (lookup-func-def da (func-app-name ex)))
                            (define def-param (func-def-parameter definition))
                            (define def-body (func-def-body definition))
                            (define evaluated-def-body
                              (local (
                                      (define (evaluate-def-body def-body def-param)
                                        (cond
                                          [(number? def-body) def-body]
                                          [(and (not (equal? def-body def-param))
                                                (symbol? def-body)) (eval-all def-body da)]
                                          [(equal? def-body def-param) def-body]
                                          [(add? def-body) (make-add (cond
                                                                       [(equal? (add-left def-body) def-param)
                                                                        def-param]
                                                                       [else (evaluate-def-body (add-left def-body) def-param)])
                                                                     (cond
                                                                       [(equal? (add-right def-body) def-param)
                                                                        def-param]
                                                                       [else (evaluate-def-body (add-right def-body) def-param)]))]
                                          [(mul? def-body) (make-mul (cond
                                                                       [(equal? (mul-left def-body) def-param)
                                                                        def-param]
                                                                       [else (evaluate-def-body (mul-left def-body) def-param)])
                                                                     (cond
                                                                       [(equal? (mul-right def-body) def-param)
                                                                        def-param]
                                                                       [else (evaluate-def-body (mul-right def-body) def-param)]))]
                                         [(func-app? def-body) 
                                                              (cond 
                                                                [(equal? (func-app-argument def-body) def-param)
                                                                 (eval-all (make-func-app (func-app-name def-body)
                                                                                          (func-app-argument ex)) da)]
                                                                [else (eval-all def-body da)])])))
                                (evaluate-def-body def-body def-param))))
                      
                      (eval-definition1 (make-func-app (func-app-name ex) (eval-all (func-app-argument ex) da))
                                        (func-app-name ex)
                                        def-param
                                        evaluated-def-body))]))

(check-expect (eval-all 'close-to-pi EXAMPLE-DA-ALL) 3.14)
(check-expect (eval-all (make-func-app 'area-of-circle 2) EXAMPLE-DA-ALL) 12.56)
(check-expect (eval-all (make-func-app 'volume-of-10-cylinder 2) EXAMPLE-DA-ALL) 125.6)

; EX 362
; S-expr SL -> Number
; parses the expression and list of definitions then uses eval-all to evaluate the expression
(define (interpreter s-expr sl)
  (local (
          (define parsed-expr (parse-func s-expr))
          (define (parse-lo-defs sl)
            (cond
              [(empty? sl) '()]
              [else (cons (parse-func (first sl)) (parse-lo-defs (rest sl)))])))
    (eval-all parsed-expr (parse-lo-defs sl))))

(check-expect (interpreter '(f 10) '((define (f s) (* (* s s) fart))(define fart 10))) 1000)

;TODO: test parse-lo-defs
  
; S-expr -> BSL-expr
(define (parse-func s)
  (cond
    [(atom? s) (parse-atom-func s)]
    [else (parse-sl-func s)]))

; SL -> BSL-expr
(define (parse-func-app s)
  (make-func-app (first s) (parse-func (second s))))
 
; SL -> BSL-expr 
(define (parse-add-mul-or-def s)
  (cond
    [(symbol=? (first s) '+)
     (make-add (parse-func (second s)) (parse-func (third s)))]
    [(symbol=? (first s) '*)
     (make-mul (parse-func (second s)) (parse-func (third s)))]
    [(and (symbol=? (first s) 'define) (list? (second s)))
     (parse-func-def s)]
    [(symbol=? (first s) 'define)
     (list (parse-func (second s)) (parse-func (third s)))]
    [else (error WRONG)]))

; SL -> BSL-expr
(define (parse-func-def s)
  (make-func-def (first (second s)) (second (second s)) (parse-func (third s))))
 
; Atom -> BSL-expr 
(define (parse-atom-func s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) s]))

; SL -> BSL-expr
(define (parse-sl-func s)
  (cond
    [(and (equal? (length s) 2) (symbol? (first s))) (parse-func-app s)]
    [(and (equal? (length s) 3) (symbol? (first s))) (parse-add-mul-or-def s)]
    [(and (equal? (length s) 4) (symbol? (first s)) (symbol? (second s)) (symbol? (third s))) (parse-func-def s)]
    [else (error WRONG)]))