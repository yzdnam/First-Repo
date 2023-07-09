#lang sicp

(define (tagged-list? exp symb)
  (and (pair? exp)
       (equal? (car exp) symb)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (if (dupe-label? next-inst labels)
                            (error "Label already exists: " next-inst)
                            (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (dupe-label? label labels)
  (cond ((null? labels) #f)
        ((equal? label (name (car labels))) #t)
        (else (dupe-label? label (cdr labels)))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (name labeled-instructions)
  (car labeled-instructions))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE"
                inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  ;(if (any-labels? (operation-exp-operands exp))
  ;    (error "Bad operand: " exp)
      (let ((op (lookup-prim (operation-exp-op exp)
                             operations))
            (aprocs
             (map (lambda (e)
                    (make-primitive-exp e machine labels))
                  (operation-exp-operands exp))))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs)))))

(define (any-labels? operands)
  (cond ((null? operands) #f)
        ((label-exp? (car operands)) #t)
        (else (any-labels? (cdr operands)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (no-more-exps? seq) (null? seq))
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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

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
                   (cddr exp))))

(define (make-definition variable value)
  (if (lambda? value)
      (cons 'define (cons (cons variable (lambda-parameters value)) (lambda-body value)))
      (cons 'define (cons variable (cons value '())))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

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
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

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

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

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

(define (make-lex-addr frame var)
  (list frame var))
(define (lex-addr-frame lex-addr)
  (car lex-addr))
(define (lex-addr-var lex-addr)
  (cadr lex-addr))

(define (find-variable var ct-env0)
  (define (accum-frames frame-pos displacement ct-env)
    (if (equal? ct-env the-empty-environment)
        'not-found
        (let ((cur-frame (car ct-env)))
          (if (memq var cur-frame)
              (accum-displacement frame-pos displacement cur-frame)
              (accum-frames (inc frame-pos) displacement (cdr ct-env))))))
  (define (accum-displacement frame-pos displacement frame)
    (if (equal? var (car frame))
        (list frame-pos displacement)
        (accum-displacement frame-pos (inc displacement) (cdr frame))))
  (accum-frames 0 0 ct-env0))
    

(define (lexical-address-lookup address env)
  (let ((frame-number (lex-addr-frame address))
        (disp-number (lex-addr-var address)))
    (if (zero? frame-number)
        (let ((cur-frame (first-frame env)))
          (let ((cur-frame-vals (frame-values cur-frame)))
            (let ((result (list-ref cur-frame-vals disp-number)))
              (if (equal? result '*unassigned*)
                  (error "Unbound variable: LOOKUP" (list-ref (frame-variables cur-frame) disp-number))
                  result))))
          (lexical-address-lookup (make-lex-addr (dec frame-number) disp-number) (enclosing-environment env)))))

(define (lexical-address-set! address val env)
  (let ((frame-number (lex-addr-frame address))
        (disp-number (lex-addr-var address)))
    (if (zero? frame-number)
        (let ((cur-frame (first-frame env)))
          (let ((cur-frame-vals (frame-values cur-frame)))
            (set-car! (memq (list-ref cur-frame-vals disp-number) cur-frame-vals) val)))
        (lexical-address-set! (make-lex-addr (dec frame-number) disp-number) val (enclosing-environment env)))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list 'list list)
        ;(list 'map map) primitive procedures cannot accept procedures as arguments. see EX 4.14, line 690 for attempted
        ; explanation
        (list 'eq? eq?)
        (list 'true? true?)
        (list 'false? false?)
        (list 'zero? zero?)
        ;(list 'for-each for-each)
        (list 'make-vector make-vector)
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
(define (get-global-environment) the-global-environment)

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define eceval-operations
  (list (list 'adjoin-arg adjoin-arg) ;
        (list 'cons cons)
        (list 'list list)
        (list 'lexical-address-lookup lexical-address-lookup)
        (list 'lexical-address-set! lexical-address-set!)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
         (list 'empty-arglist empty-arglist);
         (list 'last-operand? last-operand?);
         (list 'no-more-exps? no-more-exps?);
         (list 'prompt-for-input prompt-for-input);
         (list 'read read);
         (list 'get-global-environment get-global-environment);
         (list 'announce-output announce-output);
         (list 'user-print user-print);
         (list 'self-evaluating? self-evaluating?);
         (list 'variable? variable?);
         (list 'quoted? quoted?);
         (list 'assignment? assignment?);
         (list 'definition? definition?);
         (list 'if? if?);
         (list 'lambda? lambda?);
         (list 'begin? begin?);
         (list 'cond? cond?);
         (list 'let? let?);
         (list 'application? application?);
         (list 'lookup-variable-value lookup-variable-value);
         (list 'text-of-quotation text-of-quotation);
         (list 'lambda-parameters lambda-parameters);
         (list 'lambda-body lambda-body);
         (list 'make-procedure make-procedure);
         (list 'make-begin make-begin)
         (list 'begin-actions begin-actions);
         (list 'first-exp first-exp);
         (list 'last-exp? last-exp?);
         (list 'rest-exps rest-exps);
         (list 'if-predicate if-predicate);
         (list 'true? true?);
         (list 'if-alternative if-alternative);
         (list 'if-consequent if-consequent) ;
         (list 'assignment-variable assignment-variable) ;
         (list 'assignment-value assignment-value) ;
         (list 'set-variable-value! set-variable-value!) ;
         (list 'definition-variable definition-variable) ;
         (list 'definition-value definition-value);
         (list 'define-variable! define-variable!) ;
         (list 'cond->if cond->if);
         (list 'let->combination let->combination);
         (list 'operands operands);
         (list 'operator operator);
         (list 'no-operands? no-operands?) ;
         (list 'first-operand first-operand); 
         (list 'rest-operands rest-operands) ;
         (list 'primitive-procedure? primitive-procedure?) 
         (list 'compound-procedure? compound-procedure?) ;
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'procedure-parameters procedure-parameters) ;
         (list 'procedure-environment procedure-environment) ;
         (list 'extend-environment extend-environment)
         (list 'procedure-body procedure-body)
         (list 'cond-clauses cond-clauses)
         (list 'cond-else-clause? cond-else-clause?) 
         (list 'cond-predicate cond-predicate)
         (list 'cond-actions cond-actions))) ;

(define eceval
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '(read-eval-print-loop
        (perform (op initialize-stack))
        (perform
         (op prompt-for-input) (const ";;EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
     print-result
        (perform (op print-stack-statistics))
        (perform (op announce-output) (const ";;EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
     unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
     unknown-procedure-type
        (restore continue) ; clean up stack (from apply-dispatch)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
     signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
     eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op cond?) (reg exp))
        (branch (label ev-cond))
        (test (op let?) (reg exp))
        (branch (label ev-let))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
     ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
     ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (goto (reg continue))
     ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
     ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (goto (reg continue))
     ev-begin
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
     ev-sequence
        (assign exp (op first-exp) (reg unev))   ;;;This change and the change in ev-sequence-last-exp removes tail recursion for EX 5.28
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        ;(test (op no-more-exps?) (reg unev))
        ;(branch (label ev-sequence-end))
        ;(assign exp (op first-exp) (reg unev))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
     ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
     ev-sequence-last-exp
     ;ev-sequence-end
        (restore continue)
        (goto (label eval-dispatch))
        ;(goto (reg continue))
     ev-if
        (save exp) ; save expression for later
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch)) ; evaluate the predicate
     ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
     ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
     ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
     ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev) ; save variable for later
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch)) ; evaluate the assignment value
     ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
         (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
     ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev) ; save variable for later
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch)) ; evaluate the definition value
     ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
         (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
     ev-cond   
        (assign unev (op cond-clauses) (reg exp))
        (save continue)
        (goto (label ev-clauses))
     ev-clauses
        (assign exp (op first-exp) (reg unev))
        (test (op cond-else-clause?) (reg exp))
        (branch (label ev-cond-action))
        (save exp)
        (save unev)
        (save env)
        (assign continue (label ev-cond-pred))
        (assign exp (op cond-predicate) (reg exp))
        (goto (label eval-dispatch))
     ev-cond-pred
        (restore env)
        (restore unev)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-cond-action))
        (goto (label ev-clauses-continue))
     ev-cond-action
        (assign unev (op cond-actions) (reg exp))
        (goto (label ev-sequence))
     ev-clauses-continue
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-clauses))
;        (assign exp (op cond->if) (reg exp))
;        (goto (label eval-dispatch))
     ev-let
        (assign exp (op let->combination) (reg exp))
        (goto (label eval-dispatch))
     ev-application
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
     ev-appl-did-operator
        (restore unev) ; the operands
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val)) ; the operator
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
     ev-appl-operand-loop
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
     ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
     ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))
     apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
     primitive-apply
        (assign val (op apply-primitive-procedure)
                (reg proc)
                (reg argl))
        (restore continue)
        (goto (reg continue))
     compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment)
                (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence)))))

(define choose-proc '(cond ((zero? x) x) (else (* x 2))))

; test of (compile '((lambda (x y) 
;              (+ x y)) 
;            1 2) 
;          'val 
;          'next 
;          the-empty-environment)

(define f
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '((assign env (op get-global-environment))
(assign proc (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const +) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call5
  after-lambda2
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch7
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call8)))

; test of (compile '(((lambda (x y) 
;               (lambda (a b c d e) 
;                 ((lambda (y z) (* x y z)) 
;                  (* a b x) 
;                  (+ c d x)))) 
;             3 4) 
;            1 2 3 4 5) 
;          'val 
;          'next )
(define f1
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '((assign env (op get-global-environment))
(assign proc (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry3) (reg env))
  (goto (reg continue))
  entry3
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry5) (reg env))
  (goto (label after-lambda6))
  entry5
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const *) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lexical-address-lookup) (const (2 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch7))
  compiled-branch8
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch7
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call9
  after-lambda6
  (save continue)
  (save proc)
  (save env)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const +) (reg proc))
  (assign val (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch14
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const *) (reg proc))
  (assign val (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch10))
  compiled-branch11
  (assign continue (label after-call12))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch10
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call12
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch17
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call18
  after-lambda4
  after-lambda2
  (assign val (const 4))
  (assign argl (op list) (reg val))
  (assign val (const 3))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
  compiled-branch20
  (assign continue (label proc-return22))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return22
  (assign proc (reg val))
  (goto (label after-call21))
  primitive-branch19
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call21
  (assign val (const 5))
  (assign argl (op list) (reg val))
  (assign val (const 4))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const 3))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const 2))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch23))
  compiled-branch24
  (assign continue (label after-call25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch23
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call25)))


(define f2
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '((assign env (op get-global-environment))
(assign proc (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry3) (reg env))
  (goto (label after-lambda4))
  entry3
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (add2)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry5) (reg env))
  (goto (label after-lambda6))
  entry5
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const +) (reg proc))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch7))
  compiled-branch8
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch7
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call9
  after-lambda6
  (perform (op lexical-address-set!) (const (0 0)) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch10))
  compiled-branch11
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch10
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call12
  after-lambda4
  (assign val (const *unassigned*))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch14
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call15
  after-lambda2
  (assign val (const 4))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch17
  (assign continue (label after-call18))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call18)))

(define f3
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '((assign env (op get-global-environment))
(assign proc (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (+ * a b x y)) (reg argl) (reg env))
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 5)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch7
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call8
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 4)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch4
  (assign continue (label after-call5))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call5
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
  compiled-branch10
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call11
  after-lambda2
  (assign val (const 4))
  (assign argl (op list) (reg val))
  (assign val (const 3))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const 2))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op get-global-environment))
  (assign val (op lookup-variable-value) (const +) (reg val))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op get-global-environment))
  (assign val (op lookup-variable-value) (const -) (reg val))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
  compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14)))