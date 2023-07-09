#lang sicp

; compiler begins on line 207

(define (tagged-list? exp symb)
  (and (pair? exp)
       (equal? (car exp) symb)))

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
        'error
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
        ((equal? (car exp) '=)
         (if (in-env? (car exp) ct-env)
             (compile-application exp target linkage ct-env)
             (compile-= exp target linkage)))
        ((equal? (car exp) '*)
         (if (in-env? (car exp) ct-env)
             (compile-application exp target linkage ct-env)
             (compile-arb-*-or-+ '* exp target linkage)))
         ((equal? (car exp) '-)
          (if (in-env? (car exp) ct-env)
              (compile-application exp target linkage ct-env)
              (compile-- exp target linkage)))
          ((equal? (car exp) '+)
           (if (in-env? (car exp) ct-env)
               (compile-application exp target linkage ct-env)
               (compile-arb-*-or-+ '+ exp target linkage)))
        ((application? exp)
         (compile-application exp target linkage ct-env))
        (else
         (error "Unknown expression type: COMPILE" exp))))

; part of solution to EX 5.44
(define (in-env? exp env)
  (cond ((null? env) #f)
        ((member exp (car env)) #t)
        (in-env? exp (cdr env))))

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

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

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

(define (spread-2-arguments operand-list)
  (let ((op1 (compile (car operand-list) 'arg1 'next))
        (op2 (compile (cadr operand-list) 'arg2 'next)))
    (if (modifies-register? op2 'arg1)
        (append-instruction-sequences op1
                                      (preserving '(arg1)
                                                  op2
                                                  (make-instruction-sequence '(arg1) '() '())))
        (append-instruction-sequences op1 op2))))

(define (compile-= exp target linkage)
  (let ((args (spread-2-arguments (cdr exp))))
    (append-instruction-sequences
     args
     (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                          `((assign ,target (op =) (reg arg1) (reg arg2))))))))

(define (compile-arb-*-or-+ op exp target linkage)
  (define (compile-rest args)
    (let ((code-to-get-last-arg
           (append-instruction-sequences
            (make-instruction-sequence (list target) '(arg1)
                                       `((assign arg1 (reg ,target))))
            (preserving '(env arg1)
                        (compile (car args) 'arg2 'next)
                        (make-instruction-sequence '(arg1 arg2) (list target)
                                                   `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))
      (if (null? (cdr args))
          code-to-get-last-arg
          (append-instruction-sequences
                      code-to-get-last-arg
                      (compile-rest (cdr args))))))
  (if (= (length exp) 3)
      (let ((args (spread-2-arguments (cdr exp))))
        (append-instruction-sequences
         args
         (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                              `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))
      (let ((first-args (list (cadr exp) (caddr exp)))
            (rest-args (cdddr exp)))
        (let ((first-args-code (spread-2-arguments first-args)))
          (append-instruction-sequences
           first-args-code
           (make-instruction-sequence '(arg1 arg2) (list target)
                                      `((assign ,target (op ,op) (reg arg1) (reg arg2))))
           (compile-rest rest-args))))))

(define (compile-- exp target linkage)
  (let ((args (spread-2-arguments (cdr exp))))
    (append-instruction-sequences
     args
     (end-with-linkage linkage (make-instruction-sequence '(arg1 arg2) (list target)
                                                          `((assign ,target (op -) (reg arg1) (reg arg2))))))))

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
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
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

