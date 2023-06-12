#lang sicp

; changes to syntax for EX 5.10:
; from (assign reg-name reg-value) to (assign (reg-name reg-value))
; from (perform (op op-name) input-1 ... input-n) to (perform ((op op-name) input-1 ... input-n))
; analogous change to test

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

(define (make-stack reg)
  (let ((s (cons reg '())))
    (define (push x) (set! s (cons (car s) (cons x (cdr s)))))
    (define (pop)
      (if (null? (cdr s))
          (error "Empty stack: POP")
          (let ((top (cadr s)))
            (set! s (cons (car s) (cddr s)))
            top)))
    (define (initialize)
      (set! s (cons reg '()))
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'reg-name) (car s))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))
(define (reg-name stack) (stack 'reg-name))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stacks '())
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stacks
                       (lambda () (if (not (null? stacks))
                                      (for-each
                                       (lambda (name-stack-pair)
                                         ((cadr name-stack-pair) 'initialize))
                                       stacks))))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))
              (set! stacks
                    (cons (list name (make-stack name))
                          stacks))))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (lookup-stack name)
        (let ((val (assoc name stacks)))
          (if val
              (cadr val)
              (error name "is not assigned a stack"))))
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
              ((eq? message 'stacks) stacks)
              ((eq? message 'get-stack)
               lookup-stack)
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
(define (get-stack machine reg-name)
  ((machine 'get-stack) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts sorted-insts labels)
     (update-insts! insts labels machine)
     (list insts sorted-insts))))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() '())
      (extract-labels
       (cdr text)
       (lambda (insts sorted-insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        sorted-insts
                        (if (dupe? next-inst (map name labels))
                            (error "Label already exists: " next-inst)
                            (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        (sort-wo-dupes (make-instruction next-inst)
                                       sorted-insts)
                        labels)))))))

(define (sort-wo-dupes new-inst already-sorted-insts)
  (if (dupe? new-inst already-sorted-insts)
      already-sorted-insts
      (insert new-inst already-sorted-insts)))

;;;; TODO define insert to sort instructions for EX 5.12

(define (dupe? label labels)
  (cond ((null? labels) #f)
        ((equal? label (car labels))) #t)
        (else (dupe? label (cdr labels))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stacks (machine 'stacks))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stacks ops)))
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
         inst labels machine pc flag stacks ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stacks pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stacks pc))
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
  (car (cadr assign-instruction)))
(define (assign-value-exp assign-instruction)
  (cdr (cadr assign-instruction)))

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
  (cadr test-instruction))

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
                           (stack-inst-reg-name inst)))
        (target-stack (get-stack machine (stack-inst-reg-name inst))))
    (lambda ()
      (push target-stack (get-contents reg))
      (push target-stack (stack-inst-reg-name inst)) ;;;;
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst)))
        (target-stack (get-stack machine (stack-inst-reg-name inst))))
    (lambda ()
      (if (equal? (pop target-stack) (stack-inst-reg-name inst))
          (begin
            (set-contents! reg (pop target-stack)) 
            (advance-pc pc))
          (error "RESTORE: Last saved register does not match " reg)))))
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
(define (perform-action inst) (cadr inst))

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
  (if (any-labels? (operation-exp-operands exp))
      (error "Bad operand: " exp)
      (let ((op (lookup-prim (operation-exp-op exp)
                             operations))
            (aprocs
             (map (lambda (e)
                    (make-primitive-exp e machine labels))
                  (operation-exp-operands exp))))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs))))))

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

; EX 5.3
;  '(controller
;    sqrt-loop
;       (assign x (op read))
;       (assign guess (const 1))
;    good-enough?
;       (test (op <) (op abs) (op -) (op square) (reg guess) (reg x) (const 0.001))
;       (branch (label sqrt-done))
;       (assign (reg guess) (op average) (reg guess) (op /) (reg x) (reg guess))
;       (goto (label good-enough?))
;    sqrt-done
;       (perform (op print) (reg guess))
;       (goto (label sqrt-loop)))

; EX 5.4 and 5.7
; a.
(define expt-recur-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
    (assign (continue (label expt-done)))
    expt-loop
       (test ((op =) (reg n) (const 0)))
       (branch (label base-case))
       (save continue)
       (assign (n (op -) (reg n) (const 1)))
       (assign (continue (label after-expt)))
       (goto (label expt-loop))
    after-expt
       (restore continue)
       (assign (val (op *) (reg b) (reg val)))
       (goto (reg continue))
    base-case
       (assign (val (const 1)))
       (goto (reg continue))
    expt-done)))

(define expt-iter-machine
  (make-machine
   '(b n counter product)
   (list (list '- -) (list '* *) (list '= =))
   '(controller
     (assign (counter (reg n)))
     (assign (product (const 1)))
     expt-iter
        (test ((op =) (reg counter) (const 0)))
        (branch (label expt-iter-done))
        (assign (counter (op -) (reg counter) (const 1)))
        (assign (product (op *) (reg b) (reg product)))
        (goto (label expt-iter))
     expt-iter-done)))

(define fib-machine
  (make-machine
   '(n continue val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign (continue (label fib-done)))
     fib-loop
        (test ((op <) (reg n) (const 2)))
        (branch (label immediate-answer))
     ;; set up to compute Fib(n  1)
        (save continue)
        (assign (continue (label afterfib-n-1)))
        (save n) ; save old value of n
        (assign (n (op -) (reg n) (const 1))) ; clobber n to n-1
        (goto (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n  1)
        (restore n)
        ;(restore continue)
     ;; set up to compute Fib(n  2)
        (assign (n (op -) (reg n) (const 2)))
        ;(save continue)
        (assign (continue (label afterfib-n-2)))
        (save val) ; save Fib(n  1)
        (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n  2)
        (assign (n (reg val))) ; n now contains Fib(n  2) ;;; commented out as part of solution to Ex 5.11a.
        (restore val) ; val now contains Fib(n  1) ;;;; changed from (restore val) to (restore n) for solution to Ex 5.11a.
        (restore continue)
        (assign (val ; Fib(n  1) + Fib(n  2)
                (op +) (reg val) (reg n)))
        (goto (reg continue)) ; return to caller, answer is in val
     immediate-answer
        (assign (val (reg n))) ; base case: Fib(n) = n
        (goto (reg continue))
     fib-done)))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val)