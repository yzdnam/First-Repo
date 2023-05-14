#lang sicp

(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;(define (prime-sum-pair list1 list2)
;  (let ((a (an-element-of list1))
;        (b (an-element-of list2)))
;    (require (prime? (+ a b)))
;    (list a b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; EX 4.35
; Write a procedure an-integer-between that returns an integer between two given bounds.
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; EX 4.36
; Explain why simply replacing an-integer-between by an-integer-starting-from in the procedure in Exercise 4.35 is not an
; adequate way to generate arbitrary Pythagorean triples.

; This would not be adequate because k would increase indefinitely after each try-again attempt. i and j would never increment
; because k would never reach the end of its alternative values.

; Write a procedure for which repeatedly typing try-again would in principle eventually generate all Pythagorean triples
(define (py-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; EX 4.37
; compare the efficiency of the following procedure with its
; version in EX 4.35
(define (a-pythagorean-triple-between4.37 low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5))
                                (fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5))
                                (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

; EX 4.39
; demonstrate a faster program by reordering the restrictions
(define (multiple-dwelling2)
  (let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5))
                                (fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5))
                                (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (> miller cooper))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

; EX 4.40
; demonstrate a faster program that solves this problem based upon generating only those possibilities that are not already ruled out by previous restrictions
(define (multiple-dwelling3)
  (let ((baker (amb 1 2 3 4)) (cooper (amb 2 4))
                                (fletcher (amb 2 4)) (miller (amb 3 4 5))
                                (smith (amb 1 2 4 5)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (> miller cooper))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

(define (multiple-dwelling4)
  (let ((cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        )
    (require
      (distinct? (list cooper fletcher)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (let ((miller (amb 1 2 3 4 5)))
      (require (> miller cooper))
      (require
        (distinct? (list cooper fletcher miller)))
      (let ((smith (amb 1 2 3 4 5)))
        (require
          (distinct? (list cooper fletcher miller smith)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4 5)))
          (require (not (= baker 5)))
          (require
            (distinct? (list cooper fletcher miller smith baker)))
          (list (list 'baker baker) (list 'cooper cooper)
                (list 'fletcher fletcher) (list 'miller miller)
                (list 'smith smith)))))))

; EX 4.41
; write an ordinary scheme program to solve the multiple dwelling puzzle
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (gen-room-assgn occupant floor)
  (cons occupant floor))
(define (occupant room-assgn) (car room-assgn))
(define (floor room-assgn) (cdr room-assgn))

(define (gen-potential-assgns occ floors)
  (map (lambda (x) (gen-room-assgn occ x)) floors))
(define (room-assgn? item)
  (and (symbol? (car item)) (number? (cdr item))))
(define (add-occupant orig addition)
  (flatmap (lambda (i)
             (map (lambda (j) (if (room-assgn? j)
                                  (list i j)
                                  (cons i j)))
                  orig))
           addition))
(define (floors-only list-of-assgns)
  (map floor list-of-assgns))
(define (find-floor name assgns)
  (cond ((null? assgns) (error name " is not assigned a floor."))
        ((equal? (occupant (car assgns)) name) (floor (car assgns)))
        (else (find-floor name (cdr assgns)))))

(define (multiple-dwelling5)
  (let ((floors (enumerate-interval 1 5)))
    (let ((cooper (filter (lambda (assgn) (not (= (floor assgn) 1)))
                          (gen-potential-assgns 'cooper floors)))
          (fletcher (filter (lambda (assgn) (and (not (= (floor assgn) 1)) (not (= (floor assgn) 5))))
                            (gen-potential-assgns 'fletcher floors))))
      (let ((c-f (filter (lambda (assgns) (and (distinct? (floors-only assgns)) (not (= (abs (- (find-floor 'fletcher assgns) (find-floor 'cooper assgns))) 1))))
                         (add-occupant cooper fletcher))))
        (let ((miller (gen-potential-assgns 'miller floors)))
          (let ((c-f-m (filter (lambda (assgns) (and (distinct? (floors-only assgns)) (> (find-floor 'miller assgns) (find-floor 'cooper assgns))))
                               (add-occupant c-f miller))))
            (let ((smith (gen-potential-assgns 'smith floors)))
              (let ((c-f-m-s (filter (lambda (assgns) (and (distinct? (floors-only assgns)) (not (= (abs (- (find-floor 'smith assgns) (find-floor 'fletcher assgns))) 1))))
                                     (add-occupant c-f-m smith))))
                (let ((baker (filter (lambda (assgn) (not (= (floor assgn) 5)))
                                     (gen-potential-assgns 'baker floors))))
                  (let ((all-occs (filter (lambda (assgns) (distinct? (floors-only assgns)))
                                          (add-occupant c-f-m-s baker))))
                    all-occs))))))))))

; EX 4.42
(define (ex4.42)
  (let ((betty (amb 1 2 3 4 5)) (kitty (amb 1 2 3 4 5))
                                (ethel (amb 1 2 3 4 5)) (joan (amb 1 2 3 4 5))
                                (mary (amb 1 2 3 4 5)))
    (require
      (or (and (= kitty 2) (not (= betty 3))) (and (not (= kitty 2)) (= betty 3))))
    (require
      (or (and (= ethel 1) (not (= joan 2))) (and (not (= ethel 1)) (= joan 2))))
    (require
      (or (and (= joan 3) (not (= ethel 5))) (and (not (= joan 3)) (= ethel 5))))
    (require
      (or (and (= kitty 2) (not (= mary 4))) (and (not (= kitty 2)) (= mary 4))))
    (require
      (or (and (= mary 4) (not (= betty 1))) (and (not (= mary 4)) (= betty 1))))
    (require
      (distinct? (list betty kitty ethel joan mary)))
    (list (list 'betty betty) (list 'kitty kitty)
          (list 'ethel ethel) (list 'joan joan)
          (list 'mary mary))))

; EX 4.43
(define (gen-list)
  (amb 'maryann 'gabrielle 'lorna 'rosalind 'melissa))
  
(define (ex4.43)
  (let ((moore (amb 'maryann 'gabrielle 'lorna 'rosalind 'melissa)) (moore-boat (gen-list)) (hood (gen-list)) (hood-boat (gen-list)))
    (require (equal? moore 'maryann))
    (require (equal? moore-boat 'lorna))
    (require (equal? hood 'melissa))
    (require (equal? hood-boat 'gabrielle))
    (let ((hall (gen-list)) (hall-boat (gen-list)))
      (require (equal? hall-boat 'rosalind))
      (require (and (distinct? (list moore hood hall)) (not (equal? hall 'rosalind))))
      (let ((parker (gen-list)) (parker-boat (gen-list)))
        (require (and (distinct? (list moore hood hall parker)) (not (equal? parker 'gabrielle))))
        (require (distinct? (list parker parker-boat)))
        (require (distinct? (list parker-boat moore-boat hood-boat hall-boat)))
        (let ((downing (gen-list)) (downing-boat (gen-list)))
          (require (distinct? (list moore hood hall parker downing)))
          (require (distinct? (list downing downing-boat)))
          (require (distinct? (list parker-boat moore-boat hood-boat hall-boat downing-boat)))
          (define (gab-father-boat list-of-fathers list-of-boats)
            (if (equal? (car list-of-fathers) 'gabrielle)
                (equal? (car list-of-boats) parker)
                (gab-father-boat (cdr list-of-fathers) (cdr list-of-boats))))
          (require (gab-father-boat (list moore hood hall parker downing) (list moore-boat hood-boat hall-boat parker-boat downing-boat)))
          (list (list 'moore moore 'boat moore-boat) (list 'hood hood 'boat hood-boat) (list 'hall hall 'boat hall-boat) (list 'parker parker 'boat parker-boat)
                (list 'downing downing 'boat downing-boat)))))))

; EX4.43 will have two solutions if we are not told that Mary Ann's last name is Moore.

; EX 4.44
; write a nondeterministic program to solve the eight-queens puzzle
(define (create-queen x n)
  (cons x (an-integer-between 1 n)))
(define (row queen)
  (car queen))
(define (col queen)
  (cdr queen))
(define (queens n)
  (define (sub-queens x board)
    (if (> x n) board
        (sub-queens (inc x) (add-queen (create-queen x n) board))))
  (sub-queens 1 '()))

(define (add-queen new-queen board)
  (let ((new-board (cons new-queen board)))
    (require (safe? new-board))
    new-board))
(define (safe? b)
  (if (null? (cdr b)) #t
      (let ((first (car b)) (next (cadr b)))
        (if (or (= (abs (- (row first) (row next))) (abs (- (col first) (col next))))
                (= (col first) (col next))) #f
                                            (safe? (cons first (cddr b)))))))

(define (testx proc)
  (newline)
  (start-test proc (runtime) 1))
(define (start-test proc start-time n)
  (begin (proc)
         (if (= n 1000)
         (report (/ (/ (- (runtime) start-time) n) 1.0))
         (start-test proc start-time (inc n)))))

(define (report elapsed-time)
  (display " *** ")  (display elapsed-time))

; Parsing natural language

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define adverbs '(adverb boastfully boldly bravely briefly brightly))
(define adjectives '(adjective fat fuzzy warm hot cold wet dry happy sad sleepy))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define conjunctions '(conjunction for and nor but or yet so))

(define (parse-simple-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))
(define (parse-sentence)
  (define (maybe-extend simple-sentence)
    (amb simple-sentence
         (maybe-extend
          (list 'compound-sentence
                simple-sentence
                (parse-conjunction)))))
  (maybe-extend (parse-simple-sentence)))
(define (parse-verb)
  (cond ((memq (car *unparsed*) verbs) (list (parse-word verbs)))
        ((memq (car *unparsed*) adverbs) (if (memq (cadr *unparsed*) verbs) (append (list 'modified-verb (parse-word adverbs)) (parse-verb))
                                             (error "input violates grammar rules" (car *unparsed*) (cadr *unparsed*))))))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-verb)))

(define (parse-simple-noun-phrase)
  (append (list 'simple-noun-phrase
                (parse-word articles))
          (parse-modifiers)))
(define (parse-modifiers)
  (cond ((memq (car *unparsed*) nouns) (list (parse-word nouns)))
        ((memq (car *unparsed*) adverbs) (if (memq (cadr *unparsed*) adjectives) (cons (parse-word adverbs) (parse-modifiers))
                                             (error "input violates grammar rules" (car *unparsed*) (cadr *unparsed*))))
        ((memq (car *unparsed*) adjectives) (cons (parse-word adjectives) (parse-modifiers)))))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

; original parse-word
;(define (parse-word word-list)
;  (require (not (null? *unparsed*)))
;  (require (memq (car *unparsed*) (cdr word-list)))
;  (let ((found-word (car *unparsed*)))
;    (set! *unparsed* (cdr *unparsed*))
;    (list (car word-list) found-word)))

; parse-word for EX 4.49
; generates sentences
(define (non-0-rand x)
  (let ((initial (random x)))
    (if (zero? initial) (non-0-rand x)
        initial)))
(define (pick-rand word-list)
  (list-ref word-list (non-0-rand (length word-list))))
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  ;(require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (pick-rand word-list)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

; the solutions page has the following:

;(define (parse-word word-list) 
;   (require (not (null? *unparsed*))) 
;   (require (memq (car *unparsed*) (cdr word-list))) 
;   (let ((found-word (car *unparsed*))) 
;     (set! *unparsed* (cdr *unparsed*)) 
;     (list-amb (cdr word-list))))

; difference is that the procedure ambs through the word-list rather than picking a word from the list at random

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-conjunction)
  (list 'conjoined-sentence
        (parse-word conjunctions)
        (parse-sentence)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

(parse '(the fat cat sleeps))
(parse '(the cat sleeps and the student eats))
(parse '(the boldly fat sleepy cat briefly sleeps))

; EX 4.46
; explain why the parsing program above wouldn't work if the operands were evaluated in any order other than from left to right
; if operands were evaluated in any other order, a word other than the first word in the sentence would be evaluated by parse first and returned as the
; first object in the sentence object

; EX 4.47
; if we were to modify parse-verb-phrase to what it is in the text under this exercise, when parse is told to cycle through its results, it will never exhaust itself
; or change its result after the first try-again because parse-verb-phrase will infintely loop. if we interchange the order of expressions in the amb, then the
; procedure will never return because parse-verb-phrase will infinitely loop

; EX 4.48
; extend the grammar to include adjectives, adverbs, and compound sentences

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (make-definition variable value)
  (if (lambda? value)
      (cons 'define (cons (cons variable (lambda-parameters value)) (lambda-body value)))
      (cons 'define (cons variable (cons value '())))))
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

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))


(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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

(define (eval exp env) ((analyze exp) env))

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