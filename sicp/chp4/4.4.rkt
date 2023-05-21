#lang sicp

(define (add-assert! entries results)
  (cond ((null? entries) results)
        ((pair? (car entries)) (add-assert! (cdr entries) (cons (cons 'assert! (list (car entries))) results)))))

(define (ormap proc li)
  (cond ((null? li) #f)
        ((proc (car li)) #t)
        (else (ormap proc (cdr li)))))

;;; Dependencies for query-driver-loop follow until line 150

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-ormap proc s)
  (cond ((stream-null? s) #f)
        ((proc (stream-car s)) #t)
        (else (stream-ormap proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

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

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;;;

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
   ;  (stream-append
      (find-assertions query-pattern frame)
      (delay
        (apply-rules query-pattern frame))
   ))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (any-filters?
             conjuncts)
      (conjoin-w-filter conjuncts frame-stream)
      (conjoin4.76 conjuncts frame-stream)))

(define (conjoin-w-filter conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first (first-conjunct conjuncts))
            (rest (rest-conjuncts conjuncts)))
        (if (or (and (all-vars-bound? first frame-stream) (filter? first))
                (and (empty-conjunction? rest) (filter? first))
                (not (filter? first)))
            (conjoin-w-filter rest
                              (qeval first frame-stream))
            (conjoin-w-filter (cons (first-conjunct rest) (cons first (rest-conjuncts rest)))
                              frame-stream)))))

(define (all-vars-bound? conjunct frame-stream)
  (define (tree-walk exp frame)
    (cond ((null? exp) #t)
          ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if (not binding)
                 #f #t)))
          ((pair? exp)
           (if (tree-walk (car exp) frame)
               (tree-walk (cdr exp) frame)
               #f))
          (else #t)))
  (stream-ormap (lambda (frame) (tree-walk conjunct frame)) frame-stream))

(define test-in-alt '((not (job (? x) (computer programmer))) (supervisor (? x) (? y))))

(define (filter? conjunct)
  (let ((prefix (car conjunct)))
    (or (equal? prefix 'not) (equal? prefix 'unique) (equal? prefix 'lisp-value))))

(define (any-filters? conjuncts)
  (ormap (lambda (conjunct) (or (equal? (car conjunct) 'not) (equal? (car conjunct) 'unique) (equal? (car conjunct) 'lisp-value)))
             conjuncts))

(define (conjoin4.76 conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first-c (qeval (first-conjunct conjuncts) frame-stream)))
        (if (empty-conjunction? (rest-conjuncts conjuncts))
            (match-frames first-c frame-stream)
            (conjoin4.76 (rest-conjuncts (rest-conjuncts conjuncts))
                         (match-frames first-c (qeval (first-conjunct (rest-conjuncts conjuncts)) frame-stream)))))))
(define (match-frames con1-frame-stream con2-frame-stream) 
  (stream-flatmap
   (lambda (1st-stream-frame)
     (stream-flatmap
      (lambda (2nd-stream-frame)
        (match-if-compatible 1st-stream-frame 2nd-stream-frame (singleton-stream '())))
      con2-frame-stream))
   con1-frame-stream))
;;; TODO fix match-if-compatible
;;; test query is: (and (lives-near ?x (Bitdiddle Ben)) (job ?x ?j)))
(define (match-if-compatible f1 f2 result-frame)
  (cond ((equal? result-frame 'failed) the-empty-stream)
        ((and (null? f1) (null? f2)) (singleton-stream result-frame))
        ((null? f1) (singleton-stream (combine-frame f2 result-frame)))
        ((null? f2) (singleton-stream (combine-frame f1 result-frame)))
        (else
         (let ((first-bind (first-binding f1)))
           (let ((first-attempt (binding-in-frame (binding-variable first-bind) f2)))
             (if (equal? first-attempt first-bind)
                 (match-if-compatible (rest-bindings f1) (remove first-bind f2)
                                      (extend (binding-variable first-bind) (binding-value first-bind) result-frame))
                 (if first-attempt
                     (cond
                       ((var? (binding-value first-bind))
                        (match-if-compatible (rest-bindings f1) (remove first-attempt f2)
                                             (extend-if-poss-and-consistent
                                              (binding-variable first-bind) (binding-value first-bind) (binding-value first-attempt) result-frame)))
                       ((var? (binding-value first-attempt))
                        (match-if-compatible (rest-bindings f1) (remove first-attempt f2)
                                             (extend-if-poss-and-consistent
                                              (binding-variable first-attempt) (binding-value first-attempt) (binding-value first-bind) result-frame)))
                       (else the-empty-stream))
                     (match-if-compatible (rest-bindings f1) f2
                                          (extend (binding-variable first-bind) (binding-value first-bind) result-frame)))))))))
(define (combine-frame f1 result-frame)
  (if (null? f1) result-frame
      (let ((first-bind (first-binding f1)))
        (combine-frame (rest-bindings f1) (extend (binding-variable first-bind) (binding-value first-bind) result-frame)))))
(define (extend-if-poss-and-consistent
         first-var first-val other-val frame)
  (let ((binding (binding-in-frame first-val frame)))
    (if binding
        (let ((new-val (binding-value binding)))
          (cond ((var? other-val)
                 (extend-if-poss-and-consistent
                  first-var other-val new-val frame))
                ((equal? new-val other-val)
                 (extend first-var new-val frame))
                (else 'failed)))
        'failed)))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
;      (stream-append-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (uniquely-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (null?
          (stream-cdr
           (qeval (maybe-unique-query query)
                  (singleton-stream frame))))
         (qeval (maybe-unique-query query)
                  (singleton-stream frame))
         the-empty-stream))
  frame-stream))
(put 'unique 'qeval uniquely-asserted)

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
            frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) (scheme-report-environment 5))
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (simple-stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))
(define (add-rules-and-assertions! assertions)
  (map add-rule-or-assertion! assertions))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
      ;(stream-append-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (stream) (not (null? stream))) stream)))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-bodies exp) (map add-assertion-body exp))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (maybe-unique-query exps) (car exps))  
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove it li)
  (filter (lambda (x) (not (equal? it x))) li))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (if (or (null? frame) (null? (first-binding frame))) #f
        (let ((first (first-binding frame)))
          (if (equal? variable (binding-variable first))
              first
              (binding-in-frame variable (rest-bindings frame))))))
;  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))
(define (remove-from-frame binding frame)
  (remove binding frame))
(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))
 

(define initialize-database
  (add-rules-and-assertions! 
   '((rule (same (? x) (? x)))
     (rule (lives-near (? person-1) (? person-2))
           (and (address (? person-1) ((? town) . (? rest-1)))
                (address (? person-2) ((? town) . (? rest-2)))
                (not (same (? person-1) (? person-2)))))
     (rule (wheel ?person)
           (and (supervisor ?middle-manager ?person)
                (supervisor ?x ?middle-manager)))

     (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
     (job (Warbucks Oliver) (administration big wheel))
     (salary (Warbucks Oliver) 150000)

     (address (Aull DeWitt) (Slumerville (Onion Square) 5))
     (job (Aull DeWitt) (administration secretary))
     (salary (Aull DeWitt) 25000)
     (supervisor (Aull DeWitt) (Warbucks Oliver))

     (address (Scrooge Eben) (Weston (Shady Lane) 10))
     (job (Scrooge Eben) (accounting chief accountant))
     (salary (Scrooge Eben) 75000)
     (supervisor (Scrooge Eben) (Warbucks Oliver))

     (address (Cratchet Robert) (Allston (N Harvard Street) 16))
     (job (Cratchet Robert) (accounting scrivener))
     (salary (Cratchet Robert) 18000)
     (supervisor (Cratchet Robert) (Scrooge Eben))

     (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
     (job (Bitdiddle Ben) (computer wizard))
     (salary (Bitdiddle Ben) 60000)
     (supervisor (Bitdiddle Ben) (Warbucks Oliver))

     (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
     (job (Hacker Alyssa P) (computer programmer))
     (salary (Hacker Alyssa P) 40000)
     (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

     (address (Fect Cy D) (Cambridge (Ames Street) 3))
     (job (Fect Cy D) (computer programmer))
     (supervisor (Fect Cy D) (Bitdiddle Ben))

     (salary (Fect Cy D) 35000)
     (salary (Tweakit Lem E) 25000)

     (address (Tweakit Lem E) (Boston (Bay State Road) 22))
     (job (Tweakit Lem E) (computer technician))
     (supervisor (Tweakit Lem E) (Bitdiddle Ben))

     (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
     (job (Reasoner Louis) (computer programmer trainee))
     (salary (Reasoner Louis) 30000)
     (supervisor (Reasoner Louis) (Hacker Alyssa P))

     (can-do-job (computer wizard) (computer programmer))
     (can-do-job (computer wizard) (computer technician))
     (can-do-job (computer programmer)
                 (computer programmer trainee))
     (can-do-job (administration secretary)
                 (administration big wheel)))))
initialize-database
(define frames1 (qeval '(lives-near (? x) (Bitdiddle Ben)) (singleton-stream '())))
(define frames2 (qeval '(job (? x) (? j)) (singleton-stream '())))
(define frame1 (stream-car (qeval '(lives-near (? x) (Bitdiddle Ben)) (singleton-stream '()))))
(define frame2 (stream-car (qeval '(job (? x) (? j)) (singleton-stream '()))))
(match-frames (qeval '(supervisor (? u) (? s)) (singleton-stream '())) (qeval '(job (? s) (computer (? j))) (singleton-stream '())))
(define test-frame (stream-car (match-frames (qeval '(supervisor (? u) (? s)) (singleton-stream '()))
                                                         (qeval '(job (? s) (computer (? j))) (singleton-stream '())))))

(define test-rule (stream-car (fetch-rules '(wheel (? x)) (singleton-stream '()))))
(match-frames frames1 frames2)