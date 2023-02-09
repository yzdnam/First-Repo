#lang sicp

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

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

; EX 2.33 complete the following definitions
(define (map2.33 p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))
(define (append2.33 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length2.33 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;(map2.33 square (list 1 2 3))
;(append2.33 (list 1 2 3) (list 4 5 6))
;(length2.33 (list 1 2 3))

; EX 2.34
; complete the following procedure that evaluates a polynomial using Horner's rule
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-seq))

; EX 2.35
; redefine count-leaves as an accumulation
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define test-tree (list 1 (list 2 (list 3 4))))
;(count-leaves test-tree)

(define (count-leaves-accum x)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves-accum x) 1)) x)))
;(count-leaves-accum test-tree)

; EX 2.36
; accumulate-n takes as its 3rd argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation
; procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results
; complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
(define test-seq-seqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;(accumulate-n + 0 test-seq-seqs)

; EX 2.37
; complete the following procedures which manipulate matrices

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (list (dot-product v row))) m))

(define test-matrix (list (list 1 -1 2) (list 0 -3 1)))
(define test-vector (list 2 1 0))
;(matrix-*-vector test-matrix test-vector)

(define (transpose mat)
  (accumulate-n cons '() mat))

;(transpose test-matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (c) (dot-product row c)) cols)) m)))

(define test-matrixA (list (list 0 4 -2) (list -4 -3 0)))
(define test-matrixB (list (list 0 1) (list 1 -1) (list 2 3)))
;(matrix-*-matrix test-matrixA test-matrixB)

; EX 2.38
(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
       (iter (op result (car rest))
               (cdr rest))))
  (iter initial sequence))

(define (foldr op initial sequence)
  (accumulate op initial sequence))
; op must be commutative and associative for fold-left and accumulate (or fold-right) to produce the same results for any given
; sequence

; EX 2.39
; complete the following definitions of reverse in terms of foldl and foldr
(define (reverse2.391 sequence)
  (foldr (lambda (x y) (append y (list x))) nil sequence))
(define (reverse2.392 sequence)
  (foldl (lambda (x y) (append (list y) x)) nil sequence))
;(reverse2.391 (list 1 2 3))
;(reverse2.392 (list 1 2 3))

; required functions for EX 2.40 and beyond

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

; EX 2.40
; Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i, j) with 1 <= j < i <= n and then
; use unique pairs to simplify the definition of prime-sum-pairs given above
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs2.40 n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;(prime-sum-pairs 5)
;(prime-sum-pairs2.40 5)

; EX 2.41
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer
; n that sum to a given integer s.

; helper for 2.41
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; true if the sum of the numbers in a given triple equal a number, n
(define (trip-eq? trip n)
  (define (sum li)
    (if (null? li) 0 (+ (car li) (sum (cdr li)))))
  (if (> (length trip) 3) (error "trip-eq?: given list not a triple") (= (sum trip) n)))

(define (triple-sum n s)
  (filter (lambda (triple) (trip-eq? triple s))
          (flatmap
           (lambda (pair)
             (map (lambda (new) (cons new pair))
                  (enumerate-interval (+ 1 (car pair)) n)))
           (unique-pairs n))))

; EX 2.42
; for the queens function, a board is a list
; an empty board is an empty list
; if a queen is placed in the first column of the board from the right, a pair representing where the queen is placed
; consisting of the following will be cons'd to the board:
; -the number of rows from the top of the board, starting with 0, where the queen is placed will be the car of the pair
; -the number of columns from the right edge of the board starting with 0 where the queen is placed will be the cdr of the pair
; to illustrate, a board with queens at the following positions:
; -first column from the right, 3 rows from the top edge of the board
; -second column from the right, at the top row of the board
; will be represented as such:
; '((0 1) (2 0))
; to summarize, a queen position will be represented by a pair in the form
; '(row# column#)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 0 (- board-size 1))))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())
(define safe-test-board '((0 1) (2 0)))
(define unsafe-test-board '((1 3) (2 2) (3 1) (4 0)))

(define (safe? k positions) 
  (let ((check-queen (list-ref (reverse positions) (- k 1)))
        (check-positions (remove-i (- k 1) (reverse positions))))
    (define (check-safe queen other-queens)
      (if (not (null? other-queens))
          (let ((other-queen (car other-queens))
                (queen-y (car queen))
                (queen-x (cadr queen)))
            (cond
              [(null? other-queens) #true]
              [(or (= queen-y (car other-queen))
                   (= queen-x (cadr other-queen))
                   (= (+ queen-x queen-y) (+ (car other-queen) (cadr other-queen)))
                   (= (- queen-x queen-y) (- (cadr other-queen) (car other-queen))))  #false]
              [else (check-safe queen (cdr other-queens))]))
      #true))
      (check-safe check-queen check-positions)))

(define (remove-i k list0)
  (define (remove-i/a counter list0 list/a)
    (cond
      [(null? list0) (reverse list/a)]
      [(= counter k) (remove-i/a (inc counter) (cdr list0) list/a)]
      [else (remove-i/a (inc counter) (cdr list0) (cons (car list0) list/a))]))
  (remove-i/a 0 list0 '()))

(remove-i 2 (list 1 2 3 4 5))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row (- k 1)) rest-of-queens))

; EX 2.43
(define (queens2.43 board-size)
  (define (queen-cols2.43 k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (queen-cols2.43 (- k 1))))
            (enumerate-interval 0 (- board-size 1))))))
  (queen-cols2.43 board-size))
