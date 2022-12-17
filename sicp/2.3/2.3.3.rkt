#lang sicp

; procedures for a set represented as an unordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; EX 2.59
; implement the union-set operation for the unordered-list representation of sets
(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(element-of-set? (car set1) set2) (union-set (cdr set1) set2)]
    [else (union-set (cdr set1) (cons (car set1) set2))]))

; EX 2.60
; design procedures element-of-set?, adjoin-set, union-set, and intersection-set as if sets are represented as a list that allow for duplicates
; element-of-set? does not change
(define (adjoin-set-dupes x set)
  (cons x set))

(define (union-set-dupes set1 set2)
  (append set1 set2))

(define (intersection-set-dupes set10 set20)
  (define (int-set-dupes/a set1 set2 set-a)
    (define (remove-one x list0)
      (define (remove/a list list/a)
        (cond
          [(null? list) list/a]
          [(equal? x (car list)) (append list/a (cdr list))]
          [else (remove/a (cdr list) (cons (car list) list/a))]))
      (remove/a list0 '()))
    (cond
      [(or (null? set1) (null? set2)) set-a]
      [(element-of-set? (car set1) set2) (int-set-dupes/a (cdr set1) (remove-one (car set1) set2) (cons (car set1) set-a))]
      [else (int-set-dupes/a (cdr set1) set2 set-a)]))
  (int-set-dupes/a set10 set20 '()))

(intersection-set-dupes '(a a b c d) '(a b c d d))

                   
; the new adjoin-set, union-set, will have the square root of the big-O notation of their non-duplicate versions because element-of-set? will not
; be called for this representation. the new intersection-set will have the same big-O notation as its non-duplicate version because element-of-set? will still be
; called for each element of set1.
       
; Applications that would benefit from this representation of a set would be for data collection with an aim to analyze the frequency of outputs or programs that
; choose a random number from a set with a non-uniform probability of choosing certain numbers

; Sets as ordered lists

(define (element-of-set?-ordered x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?-ordered x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; EX 2.61
; give an implementation of adjoin-set using the ordered representation. by analogy with element-of-set?, show how to take advantage of the ordering to produce
; a procedure that requires on the average about half as many steps as with the unordered representation
(define (adjoin-set-ordered x set) ;;; TODO
  (define (adjoin-set/a set< x set>)
  ((null? set>) (append set< '(x)))
  ((= x (car set)) set)
  ((< x (car set)) (cons x set))
  (else (adjoin-set-ordered