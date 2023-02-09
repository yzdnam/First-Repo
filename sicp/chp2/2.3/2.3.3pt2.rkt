#lang sicp

;;; sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

; EX 2.63
; analyze and compare the following two procedures which convert a tree-set to a list

(define (tree->list-1 tree) ;;; θ(n*logn)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
(define (tree->list-2 tree) ;;; θ(n)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
(define tree2 (make-tree 3 (make-tree 1 '() '())
                         (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '())
                         (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)
(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))

(define (union-set-ordered set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((x1 (car set1)) (x2 (car set2)) (rest-of-set (union-set-ordered (cdr set1) (cdr set2))))
       (cond
         ((= x1 x2) (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
         ((> x1 x2) (cons x2 (union-set-ordered set1 (cdr set2))))
         ((< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2))))))))

; EX 2.65
; give θ(n) implementations of union-set and intersection-set
(define (union-set-tree tree1 tree2)
  (list->tree (union-set-ordered (tree->list-2 tree1) (tree->list-2 tree2))))

(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set-ordered (tree->list-2 tree1) (tree->list-2 tree2))))

(define test-tree1 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())))
(define test-tree2 (make-tree 2 (make-tree 1 '() '()) (make-tree 3 '() '())))

(union-set-tree test-tree1 test-tree2)
(intersection-set-tree test-tree1 test-tree2)

;;; sets and information retrieval

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; EX 2.66
; implement lookup-tree for the case where a set of records is structured as a binary tree, ordered by the numerical values of
; the keys
(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) true)
        ((< given-key (key (entry set-of-records)))
         (lookup-tree given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup-tree given-key (right-branch set-of-records)))))