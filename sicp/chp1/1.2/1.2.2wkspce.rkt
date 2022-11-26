#lang sicp

; V2 starts on line 62

; ISSUE WITH THIS VERSION: only deducts the first x coins in each list of coins one time each before checking if the remaining amount to be changed is divisible by the
; last coin

  ; creates a list of lists using each item from the in-list to initialize each list within the resulting list
  (define (init-lists li)
    (cond
      [(eqv? li '()) '()]
      [else (cons (list (car li)) (init-lists (cdr li)))]))

(define (count-change amt0 loc0)
  ; initializes the following: (list (list penny) (list nickel) (list dime) (list quarter) (list half-dollar))
  (define loloc (init-lists loc0))
  ; self-explanatory helper-function
  (define (all-but-last li)
    (cond
      [(eqv? '() (cdr li)) '()]
      [else (cons (car li) (all-but-last (cdr li)))]))
  ; self-explanatory helper-function
  (define (rotate-list li)
    (append (list-tail li (- (length li) 1)) (all-but-last li)))
  ; if all lists of coins have a length of 1, this will return the number of ways to make change for the given amount using each coin individually
  (define (addways amt loloc ways)
    (cond
      [(eqv? '() loloc) ways]
      [(eqv? '() (cdr (car loloc))) (if (integer? (/ amt (car (car loloc)))) (addways amt0 (cdr loloc) (inc ways))
                                       (addways amt0 (cdr loloc) ways))]
      [else (addways (- amt (car (car loloc))) (cons (cdr (car loloc)) (cdr loloc)) ways)]))
  ; combines two lists of lists of equal length by appending each list from the first list with its equally indexed counterpart in the second given list
  (define (combine-2-lols base-lists lists-to-be-added)
    (cond
      [(and (eqv? base-lists '()) (eqv? lists-to-be-added '())) '()]
      [else (cons (append (car base-lists) (car lists-to-be-added)) (combine-2-lols (cdr base-lists) (cdr lists-to-be-added)))])) 

  (define (count-change/a rotating-loc deducting-loc ways)
    (define check-list (car deducting-loc))
    (cond
      [(eqv? (length check-list) (length loc0)) (addways amt0 deducting-loc ways)]
      [else (count-change/a (rotate-list rotating-loc) (combine-2-lols deducting-loc (rotate-list rotating-loc)) (addways amt0 deducting-loc ways))]))
  (count-change/a loloc loloc 0))

(count-change 100 ( list 1 5 10 25 50))

(define db-init-list (list (list 1) (list 5) (list 10) (list 25) (list 50)))

  (define (rotate-list li)
    (append (list-tail li (- (length li) 1)) (all-but-last li)))

  (define (all-but-last li)
    (cond
      [(eqv? '() (cdr li)) '()]
      [else (cons (car li) (all-but-last (cdr li)))]))

  (define (combine-2-lols base-lists lists-to-be-added)
    (cond
      [(and (eqv? base-lists '()) (eqv? lists-to-be-added '())) '()]
      [else (cons (append (car base-lists) (car lists-to-be-added)) (combine-2-lols (cdr base-lists) (cdr lists-to-be-added)))])) 

; 5 variables, increment the last until it equals amount to be changed....start with 2 variables, nickels and pennies, to determine algorithm

; correctly counts ways to change an amount with nickels and pennies
(define (cc.v2 amt)
  (define (count n ways)
    (cond
      [(eqv? (* n 5) amt) (inc ways)]
      [(< (* n 5) amt) (count (inc n) (inc ways))]
      [else ways]))
  (count 0 0))

(define (cc.v3 amt)
  (define (count d ways)
    (cond
      [(eqv? (* d 10) amt) (inc ways)]
      [(< (* d 10) amt) (count (inc d) (+ ways (cc.v2 (- amt (* d 10)))))]
      [else ways]))
  (count 0 0))

(define (cc.v4 amt)
  (define (count q ways)
    (cond
      [(eqv? (* q 25) amt) (inc ways)]
      [(< (* q 25) amt) (count (inc q) (+ ways (cc.v3 (- amt (* q 25)))))]
      [else ways]))
  (count 0 0))

; TODO: delta between cc.v5 and correct count-change function is -6.
; After adding cond clause to each cc function for if the given denomination equals the to-be-changed amount, delta changes to +34.
(define (cc.v5 amt)
  (define (count hd ways)
    (cond
      [(eqv? (* hd 50) amt) (inc ways)]
      [(< (* hd 50) amt) (count (inc hd) (+ ways (cc.v4 (- amt (* hd 50)))))]
      [else ways]))
  (count 0 0))

(cc.v5 100)