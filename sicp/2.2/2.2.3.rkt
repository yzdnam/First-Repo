#lang sicp

(define (square n) (* n n))

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

(map2.33 square (list 1 2 3))
(append2.33 (list 1 2 3) (list 4 5 6))
(length2.33 (list 1 2 3))

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
(count-leaves test-tree)

(define (count-leaves-accum x)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves-accum x) 1)) x)))
(count-leaves-accum test-tree)

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
(accumulate-n + 0 test-seq-seqs)

; EX 2.37
; complete the following procedures which manipulate matrices

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (list (dot-product v row))) m))

(define test-matrix (list (list 1 -1 2) (list 0 -3 1)))
(define test-vector (list 2 1 0))
(matrix-*-vector test-matrix test-vector)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose test-matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (c) (dot-product row c)) cols)) m)))

(define test-matrixA (list (list 0 4 -2) (list -4 -3 0)))
(define test-matrixB (list (list 0 1) (list 1 -1) (list 2 3)))
(matrix-*-matrix test-matrixA test-matrixB)

; EX 2.38
  