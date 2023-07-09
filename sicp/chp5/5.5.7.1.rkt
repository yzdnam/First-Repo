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
              ((eq? message 'restart)   ;;; needed for compile-and-run procedure
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
(define (restart machine) (machine 'restart))
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
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else
         (display object))))

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

(define (make-let var-exp-list body)
  (cons 'let (cons var-exp-list body)))

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

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-and-run expression)
  (let ((compiler-call (make-label 'compiler-call)))
    (let ((insts
           (assemble
            (statements
             (append-instruction-sequences
              (make-instruction-sequence '() '(val)
                                         `((assign val (label ,compiler-call))
                                           (goto (reg exentry))
                                           ,compiler-call))
              (compile expression 'val 'return)))
            eceval)))
      (set-register-contents! eceval 'pc insts)
      (restart eceval))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (meta-apply proc argl)
  (apply proc argl))

(define primitive-procedures
  (list (list 'car car)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'caadr caadr)
        (list 'cdddr cdddr)
        (list 'cadddr cadddr)
        (list 'set-car! set-car!)
        (list 'cdr cdr)
        (list 'cddr cddr)
        (list 'cdadr cdadr)
        (list 'not (lambda (x) (not x)))
        (list 'length length)
        (list 'number? number?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'variable? variable?)
        (list 'cons cons)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list 'list list)
        ;(list 'map map) primitive procedures cannot accept procedures as arguments. see EX 4.14, line 690 for attempted
        ; explanation
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'true? true?)
        (list 'false? false?)
        (list 'zero? zero?)
        ;(list 'for-each for-each)
        (list 'make-vector make-vector)
        (list 'compile-and-run compile-and-run)
        (list 'read read)
        (list 'newline newline)
        (list 'meta-apply meta-apply)
        (list 'display display)
        ;(list 'the-mce-environment the-mce-environment)
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


(define eceval-operations
  (list (list 'adjoin-arg adjoin-arg) ;
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
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
        (list 'false? false?)
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
   '(exp env val continue proc argl unev arg1 arg2 compapp exentry)
   eceval-operations
   '((assign exentry (label external-entry))
     (assign compapp (label compound-apply))
     (branch (label external-entry))
    read-eval-print-loop
        (perform (op initialize-stack))
        (perform
         (op prompt-for-input) (const ";;EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
     external-entry
        (perform (op initialize-stack))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (reg val))
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
        ;(assign exp (op first-exp) (reg unev))   ;;;This change and the change in ev-sequence-last-exp removes tail recursion for EX 5.28
        ;(test (op last-exp?) (reg unev))
        ;(branch (label ev-sequence-last-exp))
        (test (op no-more-exps?) (reg unev))
        (branch (label ev-sequence-end))
        (assign exp (op first-exp) (reg unev))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
     ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
     ;ev-sequence-last-exp
     ev-sequence-end
        (restore continue)
        ;(goto (label eval-dispatch))
        (goto (reg continue))
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
        (test (op compiled-procedure?) (reg proc))
        (branch (label compiled-apply)) 
        (goto (label unknown-procedure-type))
     compiled-apply
        (restore continue)
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
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

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (compile-and-go expression)
  (let ((instructions
         (assemble
          (statements
           (compile expression 'val 'return))
          eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(define (compile exp target linkage)
  (inter-compile exp target linkage '()))

(define (inter-compile exp target linkage ct-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage ct-env))
        ((quoted? exp) (compile-quoted exp target linkage ct-env))
        ((variable? exp)
         (compile-variable exp target linkage ct-env))
        ((assignment? exp)
         (compile-assignment exp target linkage ct-env))
        ((definition? exp)
         (compile-definition exp target linkage ct-env))
        ((if? exp) (compile-if exp target linkage ct-env))
        ((lambda? exp) (compile-lambda exp target linkage ct-env))
        ((begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage ct-env))
        ((cond? exp)
         (compile-if (cond->if exp) target linkage ct-env))
        ((let? exp)
         (compile-application (let->combination exp) target linkage ct-env))
;        ((equal? (car exp) '=)
;         (if (in-env? (car exp) ct-env)
;             (compile-application exp target linkage ct-env)
;             (compile-= exp target linkage ct-env)))
;        ((equal? (car exp) '*)
;         (if (in-env? (car exp) ct-env)
;             (compile-application exp target linkage ct-env)
;             (compile-arb-*-or-+ '* exp target linkage ct-env)))
;         ((equal? (car exp) '-)
;          (if (in-env? (car exp) ct-env)
;              (compile-application exp target linkage ct-env)
;              (compile-- exp target linkage ct-env)))
;          ((equal? (car exp) '+)
;           (if (in-env? (car exp) ct-env)
;               (compile-application exp target linkage ct-env)
;               (compile-arb-*-or-+ '+ exp target linkage ct-env)))
          ((member (car exp) '(= * - +))
           (if (in-env? (car exp) ct-env)
               (compile-application exp target linkage ct-env)
               (compile-open-code-prim (car exp) exp target linkage ct-env)))
          ((application? exp)
           (compile-application exp target linkage ct-env))
          (else
           (error "Unknown expression type: COMPILE" exp))))

; part of solution to EX 5.44
(define (in-env? exp env)
  (cond ((null? env) #f)
        ((member exp (car env)) #t)
        (else (in-env? exp (cdr env)))))

(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage ct-env)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage ct-env)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage ct-env)
  (let ((var-addr (find-variable exp ct-env)))
    (if (equal? var-addr 'not-found)
        (end-with-linkage linkage
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target (op get-global-environment))
                                                       (assign ,target
                                                               (op lookup-variable-value)
                                                               (const ,exp)
                                                               (reg ,target)))))
        (end-with-linkage linkage
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target
                                                               (op lexical-address-lookup)
                                                               (const ,var-addr)
                                                               (reg env))))))))

(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (inter-compile (assignment-value exp) 'val 'next ct-env)))
    (let ((var-addr (find-variable var ct-env)))
      (if (equal? var-addr 'not-found)
          (end-with-linkage linkage
                            (preserving '(env)
                                        get-value-code
                                        (make-instruction-sequence '(env val) (list target)
                                                                   `((perform (op set-variable-value!)
                                                                              (const ,var)
                                                                              (reg val)
                                                                              (reg env))
                                                                     (assign ,target (const ok))))))
          (end-with-linkage linkage
                            (preserving '(env)
                                        get-value-code
                                        (make-instruction-sequence '(env val) (list target)
                                                                   `((perform (op lexical-address-set!)
                                                                              (const ,var-addr)
                                                                              (reg val)
                                                                              (reg env))
                                                                     (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (inter-compile (definition-value exp) 'val 'next ct-env)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define (compile-if exp target linkage ct-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (inter-compile (if-predicate exp) 'val 'next ct-env))
            (c-code
             (inter-compile
              (if-consequent exp) target
              consequent-linkage ct-env))
            (a-code
             (inter-compile (if-alternative exp) target linkage ct-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
      (inter-compile (first-exp seq) target linkage ct-env)
      (preserving
       '(env continue)
       (inter-compile (first-exp seq) target 'next ct-env)
       (compile-sequence (rest-exps seq) target linkage ct-env))))

(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target
                                                               (op make-compiled-procedure)
                                                               (label ,proc-entry)
                                                               (reg env)))))
        (compile-lambda-body exp proc-entry ct-env))
       after-lambda))))


(define (scan-out-defines body)

  (define (strip-vars body)
    (let ((first-exp (car body)))
      (if (not (definition? first-exp)) ; assumes any internal definitions will be the first expression in the body IAW Scheme
                                        ; implementation guidelines
          '()
          (cons (list (definition-variable first-exp) ''*unassigned*) (strip-vars (cdr body))))))
  (define (transform-body body)
    (let ((first-exp (car body)))
      (if (not (definition? (car body)))
          body
          (cons (list 'set! (definition-variable first-exp) (definition-value first-exp))
                (transform-body (cdr body))))))
  (let ((first-exp (car body)))
    (if (not (definition? (car body)))
        body
        (list (make-let (strip-vars body) (transform-body body))) )))


(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (let ((new-ct-env (cons formals ct-env)))
      (append-instruction-sequences
       (make-instruction-sequence '(env proc argl) '(env)
                                  `(,proc-entry
                                    (assign env
                                            (op compiled-procedure-env)
                                            (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
       (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return new-ct-env)))))

(define (spread-2-arguments operand-list ct-env)
  (let ((op1 (inter-compile (car operand-list) 'arg1 'next ct-env))
        (op2 (inter-compile (cadr operand-list) 'arg2 'next ct-env)))
    (if (modifies-register? op2 'arg1)
        (append-instruction-sequences op1
                                      (preserving '(arg1)
                                                  op2
                                                  (make-instruction-sequence '(arg1) '() '())))
        (append-instruction-sequences op1 op2))))

(define (compile-= exp target linkage ct-env)
  (let ((args (spread-2-arguments (cdr exp) ct-env)))
    (append-instruction-sequences
     args
     (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                          `((assign ,target (op =) (reg arg1) (reg arg2))))))))

(define (compile-arb-*-or-+ op exp target linkage ct-env)
  (define (compile-rest args)
    (let ((code-to-get-last-arg
           (append-instruction-sequences
            (make-instruction-sequence (list target) '(arg1)
                                       `((assign arg1 (reg ,target))))
            (preserving '(env arg1)
                        (inter-compile (car args) 'arg2 'next ct-env)
                        (make-instruction-sequence '(arg1 arg2) (list target)
                                                   `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))
      (if (null? (cdr args))
          code-to-get-last-arg
          (append-instruction-sequences
                      code-to-get-last-arg
                      (compile-rest (cdr args))))))
  (if (= (length exp) 3)
      (let ((args (spread-2-arguments (cdr exp) ct-env)))
       (preserving '(proc continue)
         args
         (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                              `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))
      (let ((first-args (list (cadr exp) (caddr exp)))
            (rest-args (cdddr exp)))
        (let ((first-args-code (spread-2-arguments first-args ct-env)))
          (append-instruction-sequences
           first-args-code
           (make-instruction-sequence '(arg1 arg2) (list target)
                                      `((assign ,target (op ,op) (reg arg1) (reg arg2))))
           (compile-rest rest-args))))))

(define (compile-- exp target linkage ct-env)
  (let ((args (spread-2-arguments (cdr exp) ct-env)))
    (append-instruction-sequences
     args
     (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                          `((assign ,target (op -) (reg arg1) (reg arg2))))))))

(define (compile-open-code-prim op exp target linkage ct-env)
  (let ((operand-codes
        (map (lambda (operand)
               (inter-compile operand 'val 'next ct-env))
             (operands exp)))
        (after-call (make-label 'after-call)))
    (append-instruction-sequences
     (make-instruction-sequence '() '(proc)
                                `((assign proc (op get-global-environment))
                                  (assign proc (op lookup-variable-value) (const ,op) (reg proc))))
     (preserving '(proc continue)
                 (construct-arglist operand-codes)
                 (end-with-linkage linkage
                                   (make-instruction-sequence '(proc argl)
                                                              (list target)
                                                              `((assign ,target
                                                                        (op apply-primitive-procedure)
                                                                        (reg proc)
                                                                        (reg argl)))))))))

(define (compile-application exp target linkage ct-env)
  (let ((proc-code (inter-compile (operator exp) 'proc 'next ct-env))
        (operand-codes
         (map (lambda
                  (operand) (inter-compile operand 'val 'next ct-env))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl) '(argl)
                                                '((assign argl
                                                          (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))
                                    (test (op compound-procedure?) (reg proc))
                                    (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (parallel-instruction-sequences
         (append-instruction-sequences
          compiled-branch
          (compile-proc-appl target compiled-linkage))
         (append-instruction-sequences
          compound-branch
          (compile-compound-proc-appl target compiled-linkage)))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence '(proc argl)
                                                      (list target)
                                                      `((assign ,target
                                                                (op apply-primitive-procedure)
                                                                (reg proc)
                                                                (reg argl)))))))
       after-call))))

(define (compile-compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (save continue)
                                        (goto (reg compapp))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val: COMPILE"
                target))))
 

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val: COMPILE"
                target))))

(define all-regs '(env proc val argl continue))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
                       (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

(define rcepl-operations
  (append (list (list 'compile compile)
                (list 'assemble (lambda (text) (assemble (statements text) rcepl))))
          eceval-operations))
            
(define rcepl
  (make-machine
   '(exp env val continue proc argl unev arg1 arg2 compapp exentry)
   rcepl-operations
   '(read-compile-exec-print-loop
        (perform (op initialize-stack))
        (perform
         (op prompt-for-input) (const ";;RCEPL input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (assign exp (op compile) (reg exp) (const val) (const return))
        (assign val (op assemble) (reg exp))
        (goto (reg val))
     print-result
        (perform (op print-stack-statistics))
        (perform (op announce-output) (const ";;RCEPL value:"))
        (perform (op user-print) (reg val))
        (goto (label read-compile-exec-print-loop)))))


;(compile '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))) 'val 'next)
;(compile-and-go '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))))
;(compile-and-go '(define (f x) (g x)))



(define mce-controller-txt
  (statements
   (append-instruction-sequences
    (make-instruction-sequence '() '(env)
                               '((assign env (op get-global-environment))))
    (compile
               '(begin

                  (define (map proc argl)
                    (if (null? argl) '()
                        (cons (proc (car argl)) (map proc (cdr argl)))))
                  
                  (define mce-primitive-procedures
                    (list (list 'car car)
                          (list 'cadr cadr)
                          (list 'caddr caddr)
                          (list 'set-car! set-car!)
                          (list 'cdr cdr)
                          (list 'length length)
                          (list 'number? number?)
                          (list 'string? string?)
                          (list 'symbol? symbol?)
                          (list 'pair? pair?)
                          (list 'variable? variable?)
                          (list 'cons cons)
                          (list 'set-cdr! set-cdr!)
                          (list 'null? null?)
                          (list 'list list)
                          ;(list 'map map) primitive procedures cannot accept procedures as arguments. see EX 4.14, line 690 for attempted
                          ; explanation
                          (list 'eq? eq?)
                          (list 'equal? equal?)
                          (list 'true? true?)
                          (list 'false? false?)
                          (list 'zero? zero?)
                          ;(list 'for-each for-each)
                          (list 'make-vector make-vector)
                          (list 'compile-and-run compile-and-run)
                          (list 'read read)
                          (list 'newline newline)
                          (list 'meta-apply meta-apply)
                          (list 'display display)
                          (list '= =)
                          (list '< <)
                          (list '> >)
                          (list '<= <=)
                          (list '>= >=)
                          (list '+ +)
                          (list '- -)
                          (list '/ /)
                          (list '* *)))
                  (define (mce-primitive-procedure-names)
                    (map car mce-primitive-procedures))
                  (define (mce-primitive-procedure-objects)
                    (map cadr
                         mce-primitive-procedures))

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

                  (define (define-variable! var val env)
                    (let ((frame (first-frame env)))
                      (define (scan vars vals)
                        (cond ((null? vars)
                               (add-binding-to-frame! var val frame))
                              ((eq? var (car vars)) (set-car! vals val))
                              (else (scan (cdr vars) (cdr vals)))))
                      (scan (frame-variables frame) (frame-values frame))))

                  (define (extend-environment vars vals base-env)
                    (if (= (length vars) (length vals))
                        (cons (make-frame vars vals) base-env)
                        (if (< (length vars) (length vals))
                            (error "Too many arguments supplied" vars vals)
                            (error "Too few arguments supplied" vars vals))))

                  (define (setup-mce-environment)
                    (let ((initial-env
                           (extend-environment (mce-primitive-procedure-names)
                                               (mce-primitive-procedure-objects)
                                               the-empty-environment)))
                      (define-variable! 'true true initial-env)
                      (define-variable! 'false false initial-env)
                      initial-env))

                  (define the-mce-environment (setup-mce-environment))
                  (define (get-mce-environment) the-mce-environment)


                  (define input-prompt ";;; M-Eval input:")
                  (define output-prompt ";;; M-Eval value:")

                  (define (prompt-for-input string)
                    (newline) (newline) (display string) (newline))
                  (define (announce-output string)
                    (newline) (display string) (newline))

                  (define (tagged-list? exp symb)
                    (if (pair? exp)
                        (equal? (car exp) symb)
                        false))

                  (define (compound-procedure? p)
                    (tagged-list? p 'procedure))


                  (define (self-evaluating? exp)
                    (cond ((number? exp) true)
                          ((string? exp) true)
                          (else false)))


                  (define (quoted? exp) (tagged-list? exp 'quote))
                  (define (text-of-quotation exp) (cadr exp))

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

                  (define (assignment? exp) (tagged-list? exp 'set!))
                  (define (assignment-variable exp) (cadr exp))
                  (define (assignment-value exp) (caddr exp))

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

                  (define (eval-assignment exp env)
                    (set-variable-value! (assignment-variable exp)
                                         (eval (assignment-value exp) env)
                                         env)
                    'ok)

                  (define (make-lambda parameters body)
                    (cons 'lambda (cons parameters body)))

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

                  
                  (define (eval-definition exp env)
                    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env)
                    'ok)

                  (define (if? exp) (tagged-list? exp 'if))
                  (define (if-predicate exp) (cadr exp))
                  (define (if-consequent exp) (caddr exp))
                  (define (if-alternative exp)
                    (if (not (null? (cdddr exp)))
                        (cadddr exp)
                        'false))

                  (define (eval-if exp env)
                    (if (true? (eval (if-predicate exp) env))
                        (eval (if-consequent exp) env)
                        (eval (if-alternative exp) env)))

                  (define (make-if predicate consequent alternative)
                    (list 'if predicate consequent alternative))

                  (define (lambda? exp) (tagged-list? exp 'lambda))
                  (define (lambda-parameters exp) (cadr exp))
                  (define (lambda-body exp) (cddr exp))

                  (define (make-procedure parameters body env)
                    (list 'procedure parameters body env))
                  (define (procedure-parameters p) (cadr p))
                  (define (procedure-body p) (caddr p))
                  (define (procedure-environment p) (cadddr p))

                  (define (begin? exp) (tagged-list? exp 'begin))
                  (define (begin-actions exp) (cdr exp))
                  (define (last-exp? seq) (null? (cdr seq)))
                  (define (first-exp seq) (car seq))
                  (define (rest-exps seq) (cdr seq))

                  (define (eval-sequence exps env)
                    (cond ((last-exp? exps)
                           (eval (first-exp exps) env))
                          (else
                           (eval (first-exp exps) env)
                           (eval-sequence (rest-exps exps) env))))

                  (define (sequence->exp seq)
                    (cond ((null? seq) seq)
                          ((last-exp? seq) (first-exp seq))
                          (else (make-begin seq))))
                  (define (make-begin seq) (cons 'begin seq))

                  (define (cond? exp) (tagged-list? exp 'cond))
                  (define (cond-clauses exp) (cdr exp))
                  (define (cond-else-clause? clause)
                    (eq? (cond-predicate clause) 'else))
                  (define (cond-predicate clause) (car clause))
                  (define (cond-actions clause) (cdr clause))
                  (define (cond->if exp) (expand-clauses (cond-clauses exp)))
                  (define (expand-clauses clauses)
                    (if (null? clauses)
                        false
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

                  (define (application? exp) (pair? exp))
                  (define (operator exp) (car exp))
                  (define (operands exp) (cdr exp))
                  (define (no-operands? ops) (null? ops))
                  (define (first-operand ops) (car ops))
                  (define (rest-operands ops) (cdr ops))

                  (define (list-of-values exps env)
                    (if (no-operands? exps)
                        '()
                        (cons (eval (first-operand exps) env)
                              (list-of-values (rest-operands exps) env))))

                  (define (primitive-procedure? proc)
                    (tagged-list? proc 'primitive))
                  (define (primitive-implementation proc) (cadr proc))

                  (define (apply-primitive-procedure proc args)
                    (apply-in-underlying-scheme (primitive-implementation proc) args))

                  (define apply-in-underlying-scheme meta-apply)

                  (define (apply procedure arguments)
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

                  (define (eval exp env)
                    (cond ((self-evaluating? exp) exp)
                          ((variable? exp) (lookup-variable-value exp env))
                          ((quoted? exp) (text-of-quotation exp))
                          ((assignment? exp) (eval-assignment exp env))
                          ((definition? exp) (eval-definition exp env))
                          ((if? exp) (eval-if exp env))
                          ((lambda? exp) (make-procedure (lambda-parameters exp)
                                                         (lambda-body exp)
                                                         env))
                          ((begin? exp)
                           (eval-sequence (begin-actions exp) env))
                          ((cond? exp) (eval (cond->if exp) env))
                          ((application? exp)
                           (apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env)))
                          (else
                           (error "Unknown expression type: EVAL" exp))))

                  (define (user-print object)
                    (if (compound-procedure? object)
                        (display (list 'compound-procedure
                                       (procedure-parameters object)
                                       (procedure-body object)
                                       '<procedure-env>))
                        (display object)))

 
                  (define (driver-loop)
                    (prompt-for-input input-prompt)
                    (let ((input (read)))
                      (let ((output (eval input the-mce-environment)))
                        (announce-output output-prompt)
                            (user-print output)))
                    (driver-loop))

                  (driver-loop))
               'val 'next))))


(define mce
  (make-machine
   '(exp env val continue proc argl unev arg1 arg2 compapp exentry)
   rcepl-operations
   mce-controller-txt))
