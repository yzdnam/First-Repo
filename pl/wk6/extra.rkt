#lang racket

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define (tree-height btree)
  (if (btree-leaf? btree)
      0
      (let ([left-height (+ 1 (tree-height (btree-node-left btree)))]
            [right-height (+ 1 (tree-height (btree-node-right btree)))])
        (if (> left-height right-height)
            left-height
            right-height))))

(define (sum-tree btree)
  (if (btree-leaf? btree)
      0
      (let ([l-sum (sum-tree (btree-node-left btree))]
            [r-sum (sum-tree (btree-node-right btree))])
        (+ (btree-node-value btree) l-sum r-sum))))

(define (prune-at-v t v)
  (if (btree-leaf? t)
      (btree-leaf)
      (let ([val (btree-node-value t)])
        (if (equal? val v)
            (btree-leaf)
            (btree-node val (prune-at-v (btree-node-left t) v) (prune-at-v (btree-node-right t) v))))))

(define test-tree (btree-node 1 (btree-leaf) (btree-node 1 (btree-leaf) (btree-node 4 (btree-leaf) (btree-leaf)))))
;(prune-at-v test-tree 4)

(define (well-formed-tree? v)
  (or (btree-leaf? v)
      (and (btree-node? v) (well-formed-tree? (btree-node-left v)) (well-formed-tree? (btree-node-right v)))))
;(well-formed-tree? test-tree)

(define (fold-tree f init t)
  (if (btree-leaf? t)
      init
      (f (fold-tree f (btree-node-value t) (btree-node-left t)) (fold-tree f init (btree-node-right t)))))
(fold-tree (lambda (x y) (+ x y 1)) 7
           (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))

(define (crazy-sum li)
  (letrec ([inner (lambda (li opt init)
                    (if (null? (cdr li))
                        (opt init (car li))
                        (if (number? (car li))
                            (inner (cdr li) opt (opt init (car li)))
                            (inner (cdr li) (car li) init))))])
    (inner li + 0)))

(crazy-sum (list 10 * 6 / 5 - 3))
                                   
(define (either-fold f init t-or-l)
  (cond [(list? t-or-l) (if (null? t-or-l)
                            init
                            (f (car t-or-l) (either-fold f init (cdr t-or-l))))]
        [(btree-leaf? t-or-l) init]
        [(btree-node? t-or-l) (f (either-fold f (btree-node-value t-or-l) (btree-node-left t-or-l)) (either-fold f init (btree-node-right t-or-l)))]
        [else (error "EITHER-FOLD: input type is not a list or btree")]))

(define (flatten lst)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (append (flatten (car lst)) (flatten (cdr lst)))
          (cons (car lst) (flatten (cdr lst))))))