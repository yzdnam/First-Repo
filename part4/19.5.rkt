;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; EX 322
; BT Number -> Boolean
; Determines whether a given number occurs in a BT
(define (contains-bt? abt n)
  (cond
    [(no-info? abt) #false]
    [else
     (cond
       [(equal? (node-ssn abt) n) #true]
       [else (or (contains-bt? (node-left abt) n)
                 (contains-bt? (node-right abt) n))])]))

(define bt1 (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))

(define bt2 (make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

(check-expect (contains-bt? bt1 24) #true)
(check-expect (contains-bt? bt2 71) #false)

; EX 323
; BT Number -> Symbol
; Returns the name of the BT with the given Number as its ssn
; #false if no BTs in the given BT have the given Number as an ssn
(define (search-bt abt n)
  (cond
    [(no-info? abt) #false]
    [else
     (cond
       [(equal? (node-ssn abt) n) (node-name abt)]
       [else (if (boolean? (search-bt (node-left abt) n)) (search-bt (node-right abt) n)
                 (search-bt (node-left abt) n))])]))


(check-expect (search-bt bt1 24) 'i)
(check-expect (search-bt bt2 71) #false)

; EX 324
; BT -> [List-of Number]
; returns the sequence of all ssn numbers from the BT as they show up
; from left to right when looking at the tree diagram
(define (inorder abt)
  (cond
    [(no-info? abt) '()]
    [else (append (inorder (node-left abt)) (list (node-ssn abt)) (inorder (node-right abt)))]))

(check-expect (inorder bt1) (list 15 24))
(check-expect (inorder bt2) (list 87 15))
(check-expect (inorder bt3) (list 15 12 24))

(define bt3 (make-node
  15
  'd
  NONE
  (make-node
    24 'i (make-node 12 's NONE NONE) NONE)))

; EX 325
; BT Number -> Symbol
; Returns the name of the BT with the given Number as its ssn
; NONE if no BTs in the given BT have the given Number as an ssn
; uses the BST invariant to increase efficiency
(define (search-bst abt n)
  (cond
    [(no-info? abt) 'NONE]
    [else
     (cond
       [(equal? (node-ssn abt) n) (node-name abt)]
       [else (if (> (node-ssn abt) n) (search-bst (node-left abt) n)
                 (search-bst (node-right abt) n))])]))

(check-expect (search-bst bt1 24) 'i)
(check-expect (search-bst bt2 71) 'NONE)

; EX 326
; BST Number Symbol -> BST
; inserts a node with the given N and S into the given BST
(define (create-bst abst n sym)
  (cond
    [(no-info? abst) (make-node n sym NONE NONE)]
    [else (if (> (node-ssn abst) n)
              (make-node (node-ssn abst) (node-name abst) (create-bst (node-left abst) n sym) (node-right abst))
              (make-node (node-ssn abst) (node-name abst) (node-left abst) (create-bst (node-right abst) n sym)))]))

(define EXAMPLE-BST (make-node 63 'e (make-node 29 'd (make-node 15 'b (make-node 10 'a NONE NONE) (make-node 24 'c NONE NONE)) NONE)
                               (make-node 89 'g (make-node 77 'f NONE NONE) (make-node 95 'h NONE (make-node 99 'i NONE NONE)))))

(check-expect (create-bst EXAMPLE-BST 12 'x)
              (make-node 63 'e (make-node 29 'd (make-node 15 'b (make-node 10 'a NONE (make-node 12 'x NONE NONE)) (make-node 24 'c NONE NONE)) NONE)
                               (make-node 89 'g (make-node 77 'f NONE NONE) (make-node 95 'h NONE (make-node 99 'i NONE NONE)))))

; EX 327
; [List-of [List Number Symbol]] -> BST
; produces a BST from a list of Number-Name pairs
(define (create-bst-from-list lops)
  (cond
    [(empty? lops) NONE]
    [else (create-bst (create-bst-from-list (rest lops)) (first (first lops)) (second (first lops)))]))

(check-expect (create-bst-from-list '((99 i)
  (77 f)
  (24 c)
  (10 a)
  (95 h)
  (15 b)
  (89 g)
  (29 d)
  (63 e))) EXAMPLE-BST)