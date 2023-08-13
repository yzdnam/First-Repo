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

(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

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
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([f (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? f)
               (let ([f-name (fun-nameopt (closure-fun f))]
                     [f-param (fun-formal (closure-fun f))]
                     [f-bod (fun-body (closure-fun f))]
                     [f-env (closure-env f)])
                 (if f-name
                     (eval-under-env f-bod (cons (cons f-name f) (cons (cons f-param arg) f-env)))
                     (eval-under-env f-bod (cons (cons f-param arg) f-env))))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([subexp (eval-under-env (fst-e e) env)])
           (if (apair? subexp)
               (apair-e1 subexp)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([subexp (eval-under-env (snd-e e) env)])
           (if (apair? subexp)
               (apair-e2 subexp)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([subexp (eval-under-env (isaunit-e e) env)])
           (if (aunit? subexp)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3)))

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f" (fun "iter" "l" (ifaunit (var "l") (var "l") (apair (call (var "f") (fst (var "l"))) (call (var "iter") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mupl-mapAddN" "i" (call (var "map") (fun #f "e" (add (var "e") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec (; detects any functions in the expression. if a function is found, transforms the function and calls accum to accumulate the free variables inside the
           ; function body
           [detect-f (lambda (e)
                       (cond [(var? e) e]
                             [(int? e) e]
                             [(add? e) (add (detect-f (add-e1 e)) (detect-f (add-e2 e)))]
                             [(ifgreater? e) (ifgreater (detect-f (ifgreater-e1 e)) (detect-f (ifgreater-e2 e))
                                                        (detect-f (ifgreater-e3 e)) (detect-f (ifgreater-e4 e)))]
                             [(fun? e) (fun-challenge (fun-nameopt e) (fun-formal e) (detect-f (fun-body e))
                                                      (accum (fun-body e) (set (fun-formal e) (fun-nameopt e)) (set)))]
                             [(call? e) (call (detect-f (call-funexp e)) (detect-f (call-actual e)))]
                             [(mlet? e) (mlet (mlet-var e) (detect-f (mlet-e e)) (detect-f (mlet-body e)))]
                             [(apair? e) (apair (detect-f (apair-e1 e)) (detect-f (apair-e2 e)))]
                             [(fst? e) (fst (detect-f (fst-e e)))]
                             [(snd? e) (snd (detect-f (snd-e e)))]
                             [(aunit? e) e]
                             [(isaunit? e) (isaunit (detect-f (isaunit-e e)))]
                             [(closure? e) (closure (closure-env e) (detect-f (closure-fun e)))]))]
           ; accumulates the free variables in the expression                   
           [accum (lambda (e defined-vars free-vars)
                    (cond [(var? e) (if (set-member? defined-vars (var-string e))
                                        free-vars
                                        (set-add free-vars (var-string e)))]
                          [(int? e) free-vars]
                          [(add? e) (set-union (accum (add-e1 e) defined-vars free-vars) (accum (add-e2 e) defined-vars free-vars))]
                          [(ifgreater? e) (set-union (accum (ifgreater-e1 e) defined-vars free-vars) (accum (ifgreater-e2 e) defined-vars free-vars)
                                                     (accum (ifgreater-e3 e) defined-vars free-vars) (accum (ifgreater-e4 e) defined-vars free-vars))]
                          [(fun? e) (accum (fun-body e) (set-add (set-add defined-vars (fun-formal e)) (fun-nameopt e)) (set))]
                          [(call? e) (set-union (accum (call-funexp e) defined-vars free-vars) (accum (call-actual e) defined-vars free-vars))]
                          [(mlet? e) (set-union (accum (mlet-e e) defined-vars free-vars)
                                                (accum (mlet-body e) (set-add defined-vars (mlet-var e)) free-vars))]
                          [(apair? e) (set-union (accum (apair-e1 e) defined-vars free-vars) (accum (apair-e2 e) defined-vars free-vars))]
                          [(fst? e) (accum (fst-e e) defined-vars free-vars)]
                          [(snd? e) (accum (snd-e e) defined-vars free-vars)]
                          [(aunit? e) free-vars]
                          [(isaunit? e) (accum (isaunit-e e) defined-vars free-vars)]))])
    (detect-f e)))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(fun-challenge? e) (letrec ([freevars->env (lambda (fvs)
                                            (if (set-empty? fvs)
                                                null
                                                (let* ([fv (set-first fvs)]
                                                       [rest-fvs (set-remove fvs fv)]
                                                       [poss-binding (assoc fv env)])
                                                  (if poss-binding
                                                      (cons poss-binding (freevars->env rest-fvs))
                                                      (freevars->env rest-fvs)))))])
                              (closure (freevars->env (fun-challenge-freevars e)) e))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([f (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? f)
               (let ([f-name (fun-challenge-nameopt (closure-fun f))]
                     [f-param (fun-challenge-formal (closure-fun f))]
                     [f-bod (fun-challenge-body (closure-fun f))]
                     [f-env (closure-env f)])
                 (if f-name
                     (eval-under-env-c f-bod (cons (cons f-name f) (cons (cons f-param arg) f-env)))
                     (eval-under-env-c f-bod (cons (cons f-param arg) f-env))))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([subexp (eval-under-env-c (fst-e e) env)])
           (if (apair? subexp)
               (apair-e1 subexp)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([subexp (eval-under-env-c (snd-e e) env)])
           (if (apair? subexp)
               (apair-e2 subexp)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([subexp (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? subexp)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
