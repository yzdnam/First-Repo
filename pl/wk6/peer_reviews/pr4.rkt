;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist racketlist)
  (cond [(null? racketlist) (aunit)]
        [#t (apair
             (car racketlist)
             (racketlist->mupllist (cdr racketlist)))]))

(define (mupllist->racketlist mupllist)
  (cond [(aunit? mupllist) null]
        [#t (cons
             (apair-e1 mupllist)
             (mupllist->racketlist (apair-e2 mupllist)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(int? e) e]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (or (not (int? e1)) (not (int? e2)))
               (error "One of the arguments is not int")
               (if (> (int-num e1) (int-num e2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))))]
        [(mlet? e)
         (if (not (string? (mlet-var e)))
             (error "Bad variable name")
             (eval-under-env (mlet-body e)
                             (if (null? env)
                                 (list (cons (mlet-var e) (eval-under-env (mlet-e e) env)))
                                 (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))))]
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           
           (if (not (closure? funexp))
               (error (format "Not a MUPL closure ~v" e))
               (let* ([c funexp]
                      [f (closure-fun c)]
                      [ce (closure-env c)]
                      [fname (fun-nameopt f)]
                      [fargname (fun-formal f)]
                      [fbody (fun-body f)]
                      [cenv1 (if fname
                                (cons (cons fname c) env)
                                env)]
                      [cenv2 (append ce (cons (cons fargname actual) cenv1))])
                 
                   (eval-under-env fbody cenv2))))]
        [(snd? e)
          (let ([x (eval-under-env (snd-e e) env)])
         (if (not (apair? x))
             (error "Not a MUPL pair")
             (apair-e2 x)))]
        [(fst? e)
         (let ([x (eval-under-env (fst-e e) env)])
         (if (not (apair? x))
             (error "Not a MUPL pair")
             (apair-e1 x)))]
        [(isaunit? e)
         (let ([a (eval-under-env (isaunit-e e) env)])
         (if (aunit? a)
             (int 1)
             (int 0)))]
        
        [(closure? e) e]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(aunit? e) e]
        [(fun? e) (closure env e)]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-exp e1))
      e2
      e3))

(define (mlet* lstlst e2)
  (letrec ([helper (lambda (env bs)
                     (if (null? bs)
                         env
                  (let ([cur-name (car (car bs))]
                        [cur-expval (eval-under-env (cdr (car bs)) env)])
                    (cons (cons cur-name cur-expval) (helper env (cdr bs))))))])
    (eval-under-env e2 (helper null lstlst))))
                        
                        
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define mupl-map
  (fun #f "map"
       (fun "realMapper" "xs"
            (ifeq (isaunit (var "xs")) (int 1)
                  (aunit)
                  (apair
                   (call (var "map") (fst (var "xs")))
                   (call (var "realMapper") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
