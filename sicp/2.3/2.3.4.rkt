#lang sicp

;;; Huffman Encoding Trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
(define (decode-1 bits current-branch)
  (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; EX 2.67
; decode the following sample tree and code
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; EX 2.68
; define encode-symbol which is found in the encode procedure and returns the list of bits that encodes a given symbol according
; to a given tree. include an error condition that signals if the symbol is not in the given tree at all

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (member? sym list)
  (cond
    ((null? list) false)
    ((equal? sym (car list)) true)
    (else (member? sym (cdr list)))))

(define (encode-symbol symbol tree)
  (let ((left (left-branch tree)) (right (right-branch tree)))
    (cond
      ((and (leaf? tree) (equal? symbol (car (symbols tree)))) '())
      ((member? symbol (symbols left)) (cons 0 (encode-symbol symbol left)))
      ((member? symbol (symbols right)) (cons 1 (encode-symbol symbol right)))
      (else (error "Given symbol not found in given tree")))))
; not the most efficient implementation. a more efficient implementation would first check if the tree is a leaf. if it is not,
; then it checks if the given symbol is in the symbols list of the left branch. if it is not, the procedure steps into the
; right branch without any other checks. this will eventually lead to the right-most leaf in the tree if the given symbol
; is not in the given tree. if the right-most leaf in the tree is not the given symbol, then an error message is signaled.

(encode-symbol 'D  sample-tree)
(encode (decode sample-message sample-tree) sample-tree)

; EX 2.69
; define the successive-merge procedure to be used in the generate-huffman-tree procedure
; the successive-merge procedure takes an ordered set of leaves as an argument and returns a huffman encoding tree
(define (successive-merge leaves)
  (cond
    ((null? (cdr leaves)) (car leaves))
    ((> (weight (car leaves)) (weight (cadr leaves)))
     (cond
       ((null? (cddr leaves)) (make-code-tree (cadr leaves) (car leaves)))
       ((> (weight (car leaves)) (weight (caddr leaves)))
        (successive-merge (cons (car leaves) (cons (make-code-tree (caddr leaves) (cadr leaves)) (cdddr leaves)))))
       (else (successive-merge (cons (make-code-tree (cadr leaves) (car leaves)) (cddr leaves))))))
    (else (successive-merge (cons (make-code-tree (cadr leaves) (car leaves)) (cddr leaves))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define test-pairs '((A 4) (B 2) (C 1) (D 1)))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

(define test-pairs2 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
(generate-huffman-tree test-pairs2)

; EX 2.70
; generate a huffman tree and encode the given song using the given eight-symbol alphabet
(define 2.70pairs '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define 2.70message '(GET A JOB SHA NA NA NA NA NA NA NA NA
                          GET A JOB
                          SHA NA NA NA NA NA NA NA NA
                          WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                          SHA BOOM))
(define 2.70tree (generate-huffman-tree 2.70pairs))
(length (encode 2.70message 2.70tree))

; bits required for encoding with huffman tree: 85 bits
; bits required for encoding using a fixed length code: 3 bits/letter * 36 letters in message = 108 bits

; EX 2.71 on paper

; EX 2.72
; What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to
; search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case
; where the relative frequencies of the n symbols are as described in Exercise 2.71, and give the order of growth
; (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

; since we're only considering the special case described in EX 2.71, θ for the number of steps needed to encode the most
; frequent symbol in the alphabet will be θ(1) since it will take a constant number of steps regardless of n.
; if using the more efficient implementation of encode-symbol where member? is only used on the left-branch of each node,
; the number of steps required to encode the least frequent symbol will grow linearly or θ(n).