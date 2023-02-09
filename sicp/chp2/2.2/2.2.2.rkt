#lang sicp

(define (square n) (* n n))

; EX 2.24 on paper

; EX 2.25
; give combinations of cars and cdrs that will pick 7 from the following lists:
; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))
(define list-a '(1 3 (5 7) 9))
(define list-c '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-c))))))))))))

; EX 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

; EX 2.27
; design a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed
(define (deep-reverse li)
  (define (deep-reverse/a li0 li/a)
    (cond
      [(null? li0) li/a]
      [(pair? (car li0)) (deep-reverse/a (cdr li0) (cons (deep-reverse (car li0)) li/a))]
      [else (deep-reverse/a (cdr li0) (cons (car li0) li/a))]))
  (deep-reverse/a li '()))

(define 2.27x (list (list 1 2) (list 3 4)))
(deep-reverse 2.27x)

; EX 2.28
; write a procedure fringe that takes as argument a tree and returns a list whose elements are all the leaves of the tree arranged in left-to-right order
(define (fringe li)
  (define (fringe/a li0 li/a)
    (cond
      [(null? li0) li/a]
      [(pair? (car li0)) (fringe/a (cdr li0) (append (fringe (car li0)) li/a))]
      [else (fringe/a (cdr li0) (cons (car li0) li/a))]))
  (fringe/a (reverse li) '()))

(fringe (list 2.27x (reverse 2.27x)))

; EX 2.29
; a binary mobile consists of two branches, a left and right one. each branch is a rod of a certain length from which hangs either a weight or another binary mobile
(define (make-mobile left right)
  (list left right))

; a length must be a number. a structure may be either a number representing a simple weight or another mobile
(define (make-branch length structure)
  (list length structure))

; write the corresponding selectors left-branch
(define (left-branch mobile)
  (car mobile))

; right-branch
(define (right-branch mobile)
  (car (cdr mobile)))

; branch-length
(define (branch-length branch)
  (car branch))

; branch-structure
(define (branch-structure branch)
  (car (cdr branch)))

; using the above selectors, define a procedure total-weight that returns the total weight of a mobile
(define (total-weight mbl)
  (let ((lstruct (branch-structure (left-branch mbl)))
        (rstruct (branch-structure (right-branch mbl))))
    (cond
      [(and (number? lstruct) (number? rstruct)) (+ lstruct rstruct)]
      [(number? lstruct) (+ lstruct (total-weight rstruct))]
      [(number? rstruct) (+ rstruct (total-weight lstruct))]
      [else (+ (total-weight lstruct) (total-weight rstruct))])))

(define test-mbl (make-mobile  (make-branch 10 10) (make-branch 10 (make-mobile (make-branch 10 (make-mobile (make-branch 10 10) (make-branch 10 10)))
                                                                                (make-branch 10 10)))))
(total-weight test-mbl)

; a mobile is balanced if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side
; and if each of the submobiles hanging off its branches is balanced
; design a predicate that tests whether a binary mobile is balanced
(define (balanced? mbl)
  (let ((l-length (branch-length (left-branch mbl)))
        (r-length (branch-length (right-branch mbl)))
        (l-struct (branch-structure (left-branch mbl)))
        (r-struct (branch-structure (right-branch mbl)))
        )
    (cond
      [(and (number? l-struct) (number? r-struct)) (= (* l-length l-struct) (* r-length r-struct))]
      [(number? l-struct) (and (= (* l-length l-struct) (* r-length (total-weight r-struct)))
                              (balanced? r-struct))]
      [(number? r-struct) (and (= (* r-length r-struct) (* l-length (total-weight l-struct)))
                              (balanced? l-struct))]
      [else (and (= (* l-length (total-weight l-struct)) (* r-length (total-weight r-struct)))
                 (balanced? r-struct)
                 (balanced? l-struct))])))

; if we change the representation of mobiles so the constructors use cons instead of list, then the selectors for the second items in each pair only need to call cdr
; instead of calling car and cdr

; EX 2.30
; Define a procedure square-tree analogous to square-list from EX 2.21 without using any higher-order procedures and also by using map 
(define (sqr-tree-direct tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (square tree)]
    [else (cons (sqr-tree-direct (car tree))
                (sqr-tree-direct (cdr tree)))]))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(sqr-tree-direct test-tree)

(define (sqr-tree-map tree)
  (map (lambda (subtree)
         (if
          (pair? subtree)
          (sqr-tree-map subtree)
          (square subtree))) tree))

(sqr-tree-map test-tree)

; EX 2.31
; abstract the answer to EX 2.30 to produce a procedure tree-map with the property that square-tree could be defined as
; (define (square-tree tree) (tree-map square tree))
(define (tree-map proc tree)
  (map (lambda (subtree)
         (if
          (pair? subtree)
          (tree-map proc subtree)
          (proc subtree))) tree))

(define (square-tree-abs tree) (tree-map square tree))
(square-tree-abs test-tree)

; EX 2.32
; complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; First, subsets will be called until it is called on a null value which will return a list containing an empty list, this is the base rest variable
; the previous calls to subsets will append this list to the result of a map function which will cons the car of the input of the current call to subsets to each of
; the lists currently contained in the rest variable 

(subsets '(1 2 3))