#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;(define var-table (make-table))
;(define lookup-variable-value (lambda (var env) ((var-table 'lookup-proc) env var)))
;(define define-variable! (lambda (var val env) ((var-table 'insert-proc!) env var val)))
;(define extend-environment
;  (lambda (vars vals env)
;    (if (or (null? (cdr vars)) (null? (cdr vals)))
;        (define-variable! (car vars) (car vals) env)
;        (begin (define-variable! (car vars) (car vals) env)
;               (extend-environment (cdr vars) (cdr vals) env)))))
; txtbook gives the definitions of the above attempted-to-be-defined functions

; SECTION 4.1 Metacircular Evaluator

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env) (cadr exp))

(put 'eval 'quote (lambda (exp env) (text-of-quotation exp env)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body
(define (make-definition variable value)
  (if (lambda? value)
      (cons 'define (cons (cons variable (lambda-parameters value)) (lambda-body value)))
      (cons 'define (cons variable (cons value '())))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; EX 4.19 work:

; POSSIBLE SOLUTION: in order to do this, have a procedure that checks each set! statement's value, if one has a term that
; evaluates to '*unassigned*, put that set! statement into a save-for-later bin and continue checking set! statements. if a set!
; statement's value does not have any terms that are '*unassigned*, then that statement will move to the final-result bin. once
; all set! statements have been checked and placed into either the save-for-later or final-result bins, run a procedure that
; checks whether any of the terms from the statements in the save-for-later bin that initially evaluated to '*unassigned* can
; be found in the final-result bin. if a statement in the save-for-later bin has all of its terms either able to be evaluated
; or defined in the final-result bin, move that statement to the end of the final-result bin. iterate this procedure on the
; two bins until the save-for-later bin is empty.

(define (andmap proc lst)
  (cond ((null? lst) #t)
        ((proc (car lst)) (andmap proc (cdr lst)))
        (else #f)))

(define (ormap proc lst)
  (cond ((null? lst) #f)
        ((proc (car lst)) #t)
        (else (ormap proc (cdr lst)))))
      
(define (filter proc lst)
    (if (null? lst) '()
        (let ((first (car lst)) (rest (cdr lst)))
          (if (proc first) (cons first (filter proc rest))
              (filter proc rest)))))

; sort internal definitions before they're scanned out and converted to assignments
; when looking at ex4.191, the goal is to reach the body of f and place a's definition before b's
; body of make-procedure should have (scan-out-defines (sort-defs body env) env)
(define (sort-defs-in-body body)
  (define (var-in-vals? var vals)
    (ormap (lambda (val) (if (pair? val)
                             (ormap (lambda (term) (eq? var term)) val)
                             (eq? var val))) vals))
  (define (sort-defs unsorted-defs independent-defs sorted-defs)
      (if (null? unsorted-defs) (append independent-defs sorted-defs)
          (let ((vars (map definition-variable unsorted-defs))
                (vals (map definition-value unsorted-defs)))
            (if (var-in-vals? (car vars) (cdr vals))
                (sort-defs (cdr unsorted-defs) independent-defs (cons (car unsorted-defs) sorted-defs))
                (sort-defs (cdr unsorted-defs) (cons (car unsorted-defs) independent-defs) sorted-defs)))))
  (let ((defs (filter definition? body))
        (non-defs (filter (lambda (term) (not (definition? term))) body)))
    (if (<= (length defs) 1)
        body
        (append (sort-defs defs '() '()) non-defs))))

  (define (defs->cascading-let body)
    (let ((first-exp (car body)))
      (if (not (definition? (car body)))
          body
          (make-let (list (list (definition-variable first-exp) '*unassigned*))
                          (cons (list 'set! (definition-variable first-exp) (definition-value first-exp))
                                (if (definition? (cadr body))
                                    (list (defs->cascading-let (cdr body)))
                                    (defs->cascading-let (cdr body))))))))

(define (scan-out-defines body env)

  (define (strip-vars body)
    (let ((first-exp (car body)))
      (if (not (definition? first-exp)) ; assumes any internal definitions will be the first expression in the body IAW Scheme
                                        ; implementation guidelines
          '()
          (cons (list (definition-variable first-exp) ''*unassigned*) (strip-vars (cdr body))))))
  (define (transform-body body env)
    (let ((first-exp (car body)))
      (if (not (definition? (car body)))
          body
          (cons (list 'set! (definition-variable first-exp) (definition-value first-exp))
                (transform-body (cdr body) env)))))
  (let ((first-exp (car body)))
    (if (not (definition? (car body)))
        body
        (list (make-let (strip-vars body) (transform-body body env))) )))
  
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines (sort-defs-in-body body) env)  env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (eq? (car (cond-actions first)) '=>)
                (make-if (cond-predicate first)
                         ((cadr (cond-actions first)) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(put 'eval 'cond (lambda (exp env) (eval4.3 (cond->if exp) env)))

(define (let? exp) (tagged-list? exp 'let))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (var-exp-list let-exp)
  (if (symbol? (cadr let-exp))
      (caddr let-exp)
      (cadr let-exp)))
(define (named-let? let-exp) (not (list? (var-exp-list let-exp))))
(define (named-let-proc named-let-exp) (cadr named-let-exp))
(define (first-varexp varexp-list) (car varexp-list))
(define (rest-varexp varexp-list) (cdr varexp-list))
(define (variable varexp) (car varexp))
(define (expression varexp) (cadr varexp))
(define (let-variables let-exp) (map variable (var-exp-list let-exp)))
(define (let-expressions let-exp) (map expression (var-exp-list let-exp)))
(define (let-body let-exp)
  (if (symbol? (cadr let-exp))
      (cdddr let-exp)
      (cddr let-exp)))

(define (varexp-list->parameters varexp-list)
  (if (null? varexp-list) '()
      (cons (variable (first-varexp varexp-list)) (varexp-list->parameters (rest-varexp varexp-list)))))
(define (varexp-list->operands varexp-list)
  (if (null? varexp-list) '()
      (cons (expression (first-varexp varexp-list)) (varexp-list->operands (rest-varexp varexp-list)))))

(define (let->combination let-exp)
  (let ((varexp-list (var-exp-list let-exp)))
    (if (named-let? let-exp)
        (make-begin (make-definition (named-let-proc let-exp) (make-lambda (varexp-list->parameters varexp-list) (let-body let-exp)))
                    (cons (named-let-proc let-exp) (varexp-list->operands varexp-list)))
        (cons (make-lambda (varexp-list->parameters varexp-list) (let-body let-exp)) (varexp-list->operands varexp-list)))))

(define (letrec->let letrec-exp)
  (define (strip-vars varexp-list)
    (map (lambda (varexp) (list (variable varexp) ''*unassigned*)) varexp-list))
  (define (transform-body ve-list body)
    (append (map (lambda (ve) (list 'set! (variable ve) (expression ve))) ve-list) body))
  (let ((ve-list (var-exp-list letrec-exp)))
    (make-let (strip-vars ve-list) (transform-body ve-list (let-body letrec-exp)))))
 
; an environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; for the following representation, each frame is a pair of lists: a list of variables and a list of their associated values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (if (eq? (car vals) '*unassigned*)
                                      (error var " is unassigned") ; for implementation of sorting
                                             ; assignments of internal definitions ; change to #false for implementation 
                                      (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (no-error-lookup var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (if (eq? (car vals) '*unassigned*)
                                      #false ;(error var " is unassigned") ; for implementation of sorting
                                             ; assignments of internal definitions ; change to #false for implementation 
                                      (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (not (eq? env the-empty-environment))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((for? exp) (eval (for->combo exp) env))
        ((application? exp)
         (mc-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        ;(list 'map map) primitive procedures cannot accept procedures as arguments. see EX 4.14, line 690 for attempted
        ; explanation
        (list 'eq? eq?)
        ;(list 'for-each for-each)
        (list 'make-vector make-vector)
        (list 'newline newline)
        (list 'display display)
        (list 'runtime runtime)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '<= <=)
        (list '>= >=)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(put 'eval 'if (lambda (exp env) (eval-if exp env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(put 'eval 'set! (lambda (exp env) (eval-assignment exp env)))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(put 'eval 'define (lambda (exp env) (eval-definition exp env)))

; EX 4.1
; write a version of list-of-values that evaluates operands from left to right regardless of the order of evaluation in the underlying Lisp. Also write a version of list-of-values that
; evaluates operands from right to left
(define (list-of-values-L->R exps env)
  (let ((first-val (eval (first-operand exps) env)))
    (if (no-operands? exps)
        '()
        (cons first-val
              (list-of-values-L->R (rest-operands exps) env)))))

(define (list-of-values-R->L exps env)
  (define (reverse-exps exps accum)
    (if (no-operands? exps) accum
        (reverse-exps (rest-operands exps) (cons (first-operand exps) accum))))
  (list-of-values-L->R (reverse-exps exps '()) env))

; EX 4.2
; a. what happens if the cond clauses in eval are reordered so that the clause for procedure applications appears before the clause for assignments

; assignments will never occur and variables will remain undefined because the evaluator will attempt to run apply on any list it is called on before checking if that list is an
; assignment

; b. make the evaluator recognize procedure applications before it checks for most other kinds of expressions by changing the syntax of the evaluated language so that procedure
; applications start with call
(define (application?4.2 exp) (tagged-list? exp 'call))
(define (operator4.2 exp) (cadr exp))
(define (operands4.2 exp) (cddr exp))

(put 'eval 'call (lambda (exp env) (apply (eval4.3 (operator exp) env)
                                          (list-of-values (operands exp) env))))

; EX 4.3
; rewrite eval so that the dispatch is done in data-directed style
(define (type exp) (car exp))
(define (contents exp) (cdr exp))

(define (eval4.3 exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (if (get 'eval (type exp))
                  ((get 'eval (type exp)) (contents exp) env)
                  (error "Unknown expression type: EVAL" exp)))))

; EX 4.4
; install and and or as new special forms for the evaluator by definining appropriate syntax procedures and evaluation procedures eval-and and eval-or. alternatively, show how to
; implement and and or as derived expressions
(define (and? exp) (tagged-list? exp 'and))
(define (and-args exp) (cdr exp))

(define (first-arg args) (car args))
(define (rest-args args) (cdr args))

(define (eval-and exp env)
  (let ((args (and-args exp)))
    (define (eval-and-args args env)
      (cond ((null? args) #t)
            ((and (null? (rest-args args)) (eval (first-arg args) env)) (eval (first-arg args) env))
            ((not (eval (first-arg args) env)) #f)
            (else (eval-and-args (rest-args args) env))))
    (eval-and-args args env)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-args exp) (cdr exp))

(define (eval-or exp env)
  (let ((args (or-args exp)))
    (define (eval-or-args args env)
      (cond ((null? args #f))
            ((eval (first-arg args) env) #t)
            (else (eval-or-args (rest-args args) env))))
    (eval-or-args args env)))

; and as derived expression
(define (and->if exp) (expand-and-args (and-args exp)))
(define (expand-and-args args)
  (if (null? args)
      'true
      (let ((first (first-arg args))
            (rest (rest-args args)))
        (if (null? rest)
                first
                (make-if first
                         (expand-and-args rest)
                         'false)))))

; or as derived expression
(define (or->if exp) (expand-or-args (or-args exp)))
(define (expand-or-args args)
  (if (null? args)
      'false
      (let ((first (first-arg args))
            (rest (rest-args args)))
        (make-if first
                 first
                 (expand-or-args rest)))))

; unless as derived expression
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-args exp) (cdr exp))

;(define (unless condition usual-value exceptional-value)
;  (if condition exceptional-value usual-value))

(define (unless->if exp)
  (make-if (cadr exp) (cadddr exp) (caddr exp)))

; EX 4.5
; modify the handling of cond so it supports an additional syntax of (<test> => <recipient>) where if <test> evaluates to a true value, then <recipient>, which must be a procedure
; of one argument, is invoked on the value of <test>
; see expand-clauses on line 118 for solution

; EX 4.6
; implement a syntactic transformation let->combination that reduces evaluating let to evaluating combinations of the type shown above, and add the appropriate clause to eval to
; handle let expressions
; see line 177

; EX 4.7
; write a procedure, let*->nested-lets that transforms a let* expression into a set of nested let expressions
(define (make-let var-exp-list body)
  (cons 'let (cons var-exp-list body)))

(define (let*->nested-lets exp)
  (define (expand-varexps var-exps)
    (if (null? var-exps)
        (let-body exp)
        (make-let (list (first-varexp var-exps)) (expand-varexps (rest-varexp var-exps)))))
  (expand-varexps (var-exp-list exp)))
; it is sufficient to add a clause to eval whose action is (eval (let*->nested-lets exp) env) because eval will expand the let*
; statement into a let statement which will be expanded by eval into a combination which will be evaluated by eval as an
; application

; EX 4.8
; "Named let" is a variant of let that has the form
; (let <var> <bindings> <body>)
; the bindings and body are just as in ordinary let, except that <var> is bound within body to a procedure whose body is <body>
; and whose parameters are the variables in the bindings. thus one can repeadedly execute the <body> by invoking the procedure
; named <var>.
; Modify let->combination of EX 4.6 to also support named let
; see let->combination for solution

; EX 4.9
; design some iteration constructs (do, for, while, and until), give examples of their use, and show how to implement them as
; derived expressions

; a do-while loop has the following syntax:
; (do <exp> while <predicate>)
; the loop executes the exp then checks the predicate. if the predicate is true, the loop runs another iteration. if it is
; false, the loop terminates
(define (do? exp)
  (tagged-list? exp 'do))

(define (do-iterating-exp do-exp)
  (cadr do-exp))
(define (do-while? do-exp)
  (let ((while-exp (cddr do-exp)))
    (and
     (not (null? while-exp))
     (tagged-list? while-exp 'while))))
(define (while-predicate do-exp)
  (cadddr do-exp))

(define (do->combo do-exp)
  (if (not (do-while? do-exp))
      (do-iterating-exp do-exp)
      (make-if (while-predicate do-exp)
               (make-begin (list (do-iterating-exp do-exp) do-exp)))))

; a for statement has the following syntax:
; (for ( init-exp cond-exp loop-exp ) iterating-stmnt)
; the init-expression is evaluated before the first iteration of the loop
; the cond-expression is checked before each iteration of the loop. if it evaluates to false, the loop exits.
; the loop-expression is evaluated before the iterating-stmnt
; once the iterating-stmnt is executed, the loop restarts by checking the cond-exp
(define (for? exp)
  (tagged-list? exp 'for))

(define (make-for icl iter)
  (list 'for icl iter))

(define (for-icl-exps for-exp)
  (cadr for-exp))

(define (none? exp)
  (eq? 'none exp))

(define (for-init-exp for-exp)
  (let ((for-exps (for-icl-exps for-exp)))
    (if (eq? (car for-exps) ':)
        'none
        (car for-exps))))

(define (for-cond-exp for-exp)
  (let ((for-exps (for-icl-exps for-exp)))
    (if (none? (for-init-exp for-exp))
        (if (or (none? (cadr for-exps)) (eq? (cadr for-exps) ':))
            'none
            (cadr for-exps))
        (if (eq? (caddr for-exps) ':)
            'none
            (caddr for-exps)))))

(define (for-loop-exp for-exp)
  (let ((for-exps (for-icl-exps for-exp))
        (init-exp (for-init-exp for-exp))
        (cond-exp (for-cond-exp for-exp)))
    (cond ((and (none? init-exp) (none? cond-exp))
           (if (null? (cddr for-exps))
               'none
               (caddr for-exps)))
          ((or (and (none? init-exp) (not (none? cond-exp)))
               (and (not (none? init-exp)) (none? cond-exp)))
           (if (null? (cdddr for-exps))
               'none
               (cadddr for-exps)))
          (else (if (null? (cddddr for-exps))
                    'none
                    (list-ref for-exps 4))))))

(define (for-iterating-stmnt for-exp)
  (caddr for-exp))

(define (for->combo for-exp)
  (let ((init-exp (for-init-exp for-exp))
        (cond-exp (for-cond-exp for-exp))
        (loop-exp (for-loop-exp for-exp))
        (iter-stmnt (for-iterating-stmnt for-exp)))
    (if (none? init-exp)
        (if (or (none? cond-exp) cond-exp)
            (if (none? loop-exp)
                (make-begin (list iter-stmnt (make-for (list ': cond-exp ':) iter-stmnt)))
                (make-begin (list iter-stmnt loop-exp (make-for (list ': cond-exp ': loop-exp) iter-stmnt)))))
        (if (or (none? cond-exp) cond-exp)
            (if (none? loop-exp)
                (make-begin (list init-exp iter-stmnt (make-for (list ': cond-exp ':) iter-stmnt)))
                (make-begin (list init-exp iter-stmnt loop-exp (make-for (list ': cond-exp ': loop-exp) iter-stmnt))))))))

; an until statment has the following syntax:
; (until cond-exp iterating-stmnt)
; the cond-exp is checked before the iterating stmnt is executed. if the cond-exp evaluates to #f, then the iterating
; statement executes. if the cond-exp evaluates to #t, then the loop terminates.
(define (until? exp) (tagged-list? exp 'until))

(define (until-cond-exp until-exp) (cadr until-exp))
(define (until-iter-stmnt until-exp) (caddr until-exp))

(define (until->if until-exp)
  (make-if (not (until-cond-exp until-exp))
           (make-begin (list (until-iter-stmnt until-exp) until-exp))
           'false))

; EX 4.10
; design and implement a new syntax for Scheme by modifying the procedures in this section, without changing eval or apply

; we could modify the syntax so the function called is at the end of expressions instead of at the front.

; EX 4.11
; rewrite environment operations so frames are represented as lists of bindings, where each binding is a name-value pair
(define (make-frame4.11 variables values)
  (map cons variables values))
(define (frame-variables4.11 frame) (map car frame))
(define (frame-values4.11 frame) (map cdr frame))
(define (add-binding-to-frame!4.11 var val frame)
  (cons (cons var val) frame))
; only the above operations needed to be modified due to data abstraction

; EX 4.12
; redefine set-variable-value!, define-variable!, and lookup-variable-value in terms of abstractions that capture the common
; patterns for traversing the environment structure
(define (env-loop-and-exec env var eq-proc null-frame-proc empty-env-proc)
  (define (scan vars vals)
    (cond ((null? vars)
           (null-frame-proc env))
          ((eq? var (car vars)) (eq-proc vals)) 
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (empty-env-proc)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value-abs var env)
  (env-loop-and-exec env var car 
                     (lambda (env) (lookup-variable-value-abs var (enclosing-environment env)))
                     (lambda () (error "Unbound variable" var))))

(define (set-variable-value!-abs var val env)
  (env-loop-and-exec env var (lambda (vals) (set-car! vals val)) 
                     (lambda (env) (set-variable-value!-abs var val (enclosing-environment env)))
                     (lambda () (error "Unbound variable: SET!" var))))

(define (define-variable!-abs var val env)
  (env-loop-and-exec env var (lambda (vals) (set-car! vals val)) 
                     (lambda (env) (add-binding-to-frame! var val (first-frame env)))
                     (lambda () (extend-environment (list var) (list val) env))))
      
; EX 4.13
; Implement for the evaluator a special form make-unbound! that removes the binding of a given symbol from the environment in
; which the make-unbound! expression is evaluated. make-unbound! will remove the binding of a given symbol from the environment
; in which it is invoked. this design choice maximizes modularity, flexibility, and precision in the use of the procedure.
; a make-unbound-in-all-enclosing-envs procedure can be created using the following procedure and an iterating loop.
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (let ((vars (frame-variables frame)) (vals (frame-values frame)))
      (define (scan vars vals)
        (if (not (null? vars))
            (if (eq? (car vars) var) (begin (set! vars (cdr vars)) (set! vals (cdr vals)))
                (scan (cdr vars) (cdr vals)))))
      (scan vars vals))))

(define (make-unbound-in-all-enclosing-envs var env)
  (if (not (eq? env the-empty-environment))
      (begin (make-unbound! env) (make-unbound-in-all-enclosing-envs var (enclosing-environment env)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

; EX 4.14
; explain why installing the system version of map as a primitive for the metacircular evaluator doesn't work while defining map
; within the evaluator does work.
;
; when map is installed as a primitive for the MCE, its arguments are evaluated with the list-of-values procedure. the first
; argument to map is always a procedure. when the procedure given to map is evaluated using list-of-values and then passed to
; apply-in-underlying-scheme as a member of the arguments list, it is not re-evaluated by the MCE and is left in its raw form
; which can't be applied to the subsequent arguments in the list.
; when map is defined within the MCE, it is applied as a compound procedure by mc-apply and its body is evaluated with its
; parameters replaced by the given arguments

; EX 4.15
; Given a one-argument procedure p and an object a, p is said to “halt” on a if evaluating the expression (p a) returns a value
; (as opposed to terminating with an error message or running forever). Show that it is impossible to write a procedure halts?
; that correctly determines whether p halts on a for any procedure p and object a. Use the following reasoning: If you had such
; a procedure halts?, you could implement the following program:

; (define (run-forever) (run-forever))
; (define (try p)
;   (if (halts? p p) (run-forever) 'halted))

; Now consider evaluating the expression (try try) and show that any possible outcome (either halting or running forever)
; violates the intended behavior of halts?.

; a halts? procedure will never evaluate to false because if a procedure and object meet the condition required for halts? to
; return false (i.e. result in an error or endless loop) the procedure/object combo will never give halts? the opportunity to
; return

; EX 4.16
; implement the method for transforming internal definitions described on page 528 of the text
; a. change lookup-variable-value to signal an error if the value it finds is the symbol '*unassigned*
; see lookup-variable-value, line 185

; b. Write a procedure scan-out-defines that takes a procedure body and returns an equivalent one that has no internal
; definitions, by making the transformation described on page 528 of the text.
; see line 94 for definition

; c. Install scan-out-defines in the interpreter, either in make-procedure or in procedure-body (see Section 4.1.3). Which
; place is better? Why?
; it is more appropriate to place scan-out-defines in make-procedure so that it is only called once when creating a new
; procedure and not every time the procedure-body function is called on the procedure

; EX 4.17
; see handwritten notes for answers to first parts of question.
; Design a way to make the interpreter implement the “simultaneous” scope rule for internal definitions without constructing
; the extra frame.
; solution: define a procedure that takes a procedure application with internal definitions that have been scanned out.
; the variables of the let expression need to be cons'd onto the list of parameters for the operator and the corresponding
; '*unassigned values need to be cons'd onto the operands. the ultimate assignments for the let-variables do not need to be
; cons'd onto the body of the operator because they will already be there from scan-out-defines

(define (simul-scop application)
  (let ((in-operator (operator application)) (in-operands (operands application)))
    (let ((operator-body (car (lambda-body in-operator))))
      (if (not (let? operator-body))
          application
          (cons (make-lambda (append (let-variables operator-body) (lambda-parameters in-operator)) (let-body operator-body))
                (append (let-expressions operator-body) in-operands))))))
; simul-scop could be installed in the MCE by adding a let expression to the application clause of eval that would assign
; a variable to the given application with simul-scop applied to it.

; EX 4.18
; the given procedure does not work if the definitions are scanned out as shown in the exercise:

;(define (solve0 f y0 dt)
;  (let ((y '*unassigned*)
;        (dy '*unassigned*))
;    (let ((a (integral3.77 (delay dy) y0 dt))
;          (b (stream-map f y)))
;      (set! y a)
;      (set! dy b)
;      y)))

; but it works if the definitions are scanned out as shown earlier in the chapter.

;(define (solve f y0 dt)
;  (let ((y '*unassigned*)
;        (dy '*unassigned*))
;    (set! y (integral3.77 (delay dy) y0 dt))
;    (set! dy (stream-map f y))
;    y))

; Explain.
; the 2nd let expression in the solve procedure shown in the exercise is equivalent to a lambda expression with parameters being
; a and b, the body remaining the body, and the integral3.77 and stream-map applications being the inputs to the lambda. when
; the evaluator attempts to evaluate the 2nd let expression (the one with a and b as its variables), it will convert it to
; an application and attempt to evaluate the integral3.77 and stream-map applications as the operands to the application. since
; these procedure applications have not yet been assigned to their ultimate variables and are also dependent on each other
; once assigned to those variables, the body of the let expression fails to execute. this isn't an issue if the definitions
; are scanned out as shown earlier in the chapter because y is set to the integral3.77 application, which uses a delayed dy,
; and then dy is set to the stream-map application which uses y and since y has just been assigned, no error arises during
; execution.

; EX 4.19
(define ex4.190
  '(let ((a 1))
     (define (f x)
       (define b (+ a x))
       (define a 5)
       (+ a b))
     (f 10)))

(define ex4.191 (let->combination ex4.190))
; (make-procedure (lambda-parameters ex4.191) (lambda-body ex4.191) the-global-environment)
; TODO fix scan-out-defines so that the MCE can process ex4.190
; since scan-out-defines returns a let-expression when given a lambda expression with internal definitions and then passes that
; to mc-apply without adding let to the environment, the "unbound variable let" error is signalled.
; a solution would be to have scan-out-defines pass it's result back to eval so that the let-expression can be converted to
; a lambda which will be able to be acted upon by mc-apply
; DONE
; now devise a way to implement internal definitions so that the value 5 for a in the procedure above is used in evaluating b
; ie internal definitions are evaluated in truly simultaneous fashion.
; TODO at line 105
(defs->cascading-let  '((define a 5) (define b (+ a x)) (+ a b)) )
(define ex4.192 '(let ((f *unassigned*)) (set! f (lambda (x) (define b (+ a x)) (define a 5) (+ a b))) (f 10)))
(define ex4.193 (assignment-value (car (lambda-body (car (let->combination ex4.192))))))
(scan-out-defines '((define a 5) (define b (+ a x)) (+ a b)) the-global-environment)
; ex4.191 is an application so when it is eval'd, the operator and the operand are eval'd. the operator successfully eval's to
; a procedure and the operand is a number. TODO follow the computation through the mc-apply procedure with the eval'd operator
; and operand
(define ex4.191body (car ex4.191))
; eval is called on the operator of ex4.191
; (eval '(lambda (a) (define (f x) (define b (+ a x)) (define a 5) (+ a b)) (f 10)) the-global-environment)
; a procedure is created:
; (procedure
; (a)
; ((let ((f *unassigned*)) (set! f (lambda (x) (define b (+ a x)) (define a 5) (+ a b))) (f 10))) the-global-environment)
; mc-apply applies the procedure to the operand:
;(eval-sequence
; '((let ((f *unassigned*)) (set! f (lambda (x) (define b (+ a x)) (define a 5) (+ a b))) (f 10))) 
; (extend-environment
;  '(a)
;  '(1)
;  the-global-environment))
(define ex-env  (extend-environment
  '(a)
  '(1)
  the-global-environment))
; eval-sequence evals the (let ((f *unassigned...) function with a as 1:
 (eval '(let ((f '*unassigned*)) (set! f (lambda (x) (define b (+ a x)) (define a 5) (+ a b))) (f 10)) ex-env)
; the given function is transformed:
; (let->combination '(let ((f '*unassigned*)) (set! f (lambda (x) (define b (+ a x)) (define a 5) (+ a b))) (f 10)))
; *unassigned needed double quotes in the scan-out-defines function to retain its quote when the resulting let function was
; produced

;;; walkthrough of xdavidliu's solution to ex4.19

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (delete x set)
  (cond ((null? set) '())
        ((equal? x (car set)) (delete x (cdr set)))
        (else (cons (car set) (delete x (cdr set))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; unrolls nested lists 
 (define (tree->list tree) 
   (if (list? tree) 
       (apply-in-underlying-scheme 
        append 
        (map tree->list tree)) 
       (list tree))) 
  
 ;; removes duplicates 
 (define (list->set lst) 
   (if (or (null? lst) 
           (null? (cdr lst))) 
       lst 
       (cons (car lst) 
             (delete (car lst) 
                     (list->set (cdr lst)))))) 
  
 (define (all-included-symbols symbol-pool seq) 
   (intersection-set symbol-pool 
                     (list->set (tree->list seq)))) 
 ;; intersection-set is given in chapter 2 of SICP

(define (find proc set)
  (cond ((null? set) #f)
        ((proc (car set)) #t)
        (else (find proc (cdr set)))))

(define (remove proc set)
  (cond ((null? set) '())
        ((proc (car set)) (remove proc (cdr set)))
        (else (cons (car set) (remove proc (cdr set))))))
  
 ;; there are likely faster ways to do this 
 ;; computes set1 - set2 nondestructively 
 (define (difference-set set1 set2) 
   (define (in-set2? obj1) 
     (find (lambda (obj2) (eq? obj1 obj2)) 
           set2)) 
   (remove in-set2? set1))

(define var-defs '((define a 3) (define b a)))

(define symbol-pool (map definition-variable var-defs))

(map (lambda (def) 
                  (cons def (all-included-symbols symbol-pool 
                                                  (definition-value def)))) var-defs)

; (((define a 3)) ((define b a) a))
;;; end walkthrough of xdavidliu's 4.19 solution

; EX 4.20
; a. implement letrec as a derived expression by transforming a letrec expression into a let expression
; see line 242 for let section and solution

(define (f x)
  (letrec
      ( (odd? (lambda (n)
               (if (= n 0) false (even? (- n 1)))))
       (even? (lambda (n)
                (if (= n 0) true (odd? (- n 1)))))
       )
    (if (even? x) 'even 'odd)))

; EX 4.21
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
; a. devise a procedure analogous to the one above for computing fibonacci numbers
((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (fb k)  (cond ((= k 0) 0)
                          ((= k 1) 1)
                          (else (+ (fib (- k 1))
                                   (fib (- k 2)))))))) 10)
; b. complete the following definition of f
(define (f4.21 x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(define (f.orig x)
  (define (even? n)
    (if (= n 0) true (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false (even? (- n 1))))
  (even? x))

; 4.1.7 Separating Syntactic Analysis from Execution
(define (new-eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

; EX 4.22
; extend the new evaluator so it supports let expressions
; solution on line 1079

; EX 4.24
; Design and carry out some experiments to compare the speed of the original metacircular evaluator
; with the version in this section. Use your results to estimate the fraction of time that is spent in analysis versus
; execution for various procedures.
; use the runtime function to time computations

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

(define (timed-factorial-test n)
  (newline)
  (display n)
  (start-factorial-test n (runtime)))
(define (start-factorial-test n start-time)
  (begin (factorial n)
         (report-factorial (- (runtime) start-time))))
(define (report-factorial elapsed-time)
  (display " *** ")
  (display elapsed-time))