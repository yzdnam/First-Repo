#lang sicp

; EX 3.12
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
;(cdr x)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define w (append! x y))
;(cdr x)
; see notes for box-and-pointer diagram explanations

; EX 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define zz (make-cycle (list 'a 'b 'c)))
; see notes for box-and-pointer diagram
; attempting to compute (last-pair zz) results in an infinite loop

; EX 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define ww (mystery v))
; see notes for box-and-pointer diagrams for v before and after application of the procedure mystery

; EX 3.15
; see notes for box-and-pointer diagrams

; EX 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define c (list 'c))
(define cc (cons c c))
(define a (cons cc cc))
; see notes for box-and-pointer diagrams representing list structures made up of exactly 3 pairs for which count-pairs returns 3; returns 4; returns 7; and never returns.

; EX 3.17
; devise a correct version of count-pairs that returns the number of distinct pairs in any structure (hint: traverse the structure, maintain an auxiliary data structure that is used
; to keep track of which pairs have already been counted.)
(define (count-pairs3.17 x0)
  (define (count-pairs-accum x pairs-already-counted)
    (cond ((not (pair? x)) 0)
          ((memq (car x) pairs-already-counted) (count-pairs-accum (cdr x) pairs-already-counted))
          (else (inc (count-pairs-accum (cdr x) (cons (car x) pairs-already-counted))))))
  (count-pairs-accum x0 '()))

;(count-pairs3.17 a)

; EX 3.18
; write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would
; go into an infinite loop
(define (loop? x)
  (define (loop?/a check)
    (cond
      ((not (pair? check)) #f)
      ((null? check) #f)
      ((eq? (car x) (car check)) #t)
      (else (loop?/a (cdr check)))))
  (loop?/a (cdr x)))
;; only checks if a loop occurs by restarting from the first term in the given list but a loop can occur if the cdr refers back to any other term in a list that has already been read

;(loop? zz)
;(loop? a)

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))

;(loop? t2)
  
 (define xx '(a b c)) 
 (define yy '(d e f)) 
 (set-car! (cdr xx) yy) 
 (set-car! xx (cdr xx)) 
 (set-cdr! (last-pair yy) (cdr yy))

;(loop? xx)
; procedure below, has-cycle?, is attributed to fubupc from the schemewiki community
 (define (has-cycle? seq) 
  
   (define (lst-in? lst records) 
     (cond ((null? records) false) 
           ((eq? (car records) lst) true) 
           (else (lst-in? lst (cdr records))))) 
  
   (define (has-cycle-1? processed lst) 
     (cond ((not (pair? lst)) false) 
           ((lst-in? lst processed) true) 
           (else 
             (or (has-cycle-1? (cons lst processed) (car lst)) 
                 (has-cycle-1? (cons lst processed) (cdr lst)))))) 
  
   (has-cycle-1? '() seq))

;(has-cycle? xx)

; EX 3.19
; redo exercise 3.18 using an algorithm that takes only a constant amount of space
; attributed to schemewiki community
(define (loop3.19 x)
  (define (safe-cdr part)
    (if (pair? part)
        (cdr part)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
          ((not (pair? b)) #f)
          ((eq? a b) #t)
          ((eq? a (safe-cdr b)) #t)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr x) (safe-cdr (safe-cdr x))))

; EX 3.20
; diagrams in notes

; Section 3.3.2 Representing Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

; EX 3.21
; explain why the standard Lisp printer doesn't know how to make sense of the queue representation:

; because the queue representation consists of two pointers that point to the same list structure when the queue is initialized. as items to the queue are inserted, the same object
; is used to modify the front and rear pointers. as items to the queue are deleted from the front, the rear-pointer does not change but the front pointer does. if the front-ptr
; is pointing to an empty list, then the queue is empty. the rear-pointer is not considered when determining the emptiness of a queue.

; define a procedure, print-queue, that takes a queue as input and prints the sequence of items in the queue.
(define (print-queue queue)
  (car queue))

;(define q (insert-queue! (make-queue) 'a))
;(print-queue q)

; EX 3.22
; build a queue as a procedure with local state
(define (make-queue3.22)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (dispatch m)
      (cond
        ((eq? m 'print) (cons front-ptr rear-ptr))
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'set-front-ptr!) set-front-ptr!)
        ((eq? m 'set-rear-ptr!) set-rear-ptr!)
        (else (error "Undefined operation: MAKE-QUEUE" m))))
    dispatch))

(define (front-ptr3.22 queue) (queue 'front-ptr))
(define (rear-ptr3.22 queue) (queue 'rear-ptr))
(define (set-front-ptr!3.22 queue new-value) ((queue 'set-front-ptr!) new-value) queue)
(define (set-rear-ptr!3.22 queue new-value) ((queue 'set-rear-ptr!) new-value) queue)

(define (empty-queue?3.22 queue)
  (null? (front-ptr3.22 queue)))

(define (front-queue3.22 queue)
  (if (empty-queue?3.22 queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr3.22 queue))))

(define (insert-queue!3.22 queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue?3.22 queue)
           (set-front-ptr!3.22 queue new-pair)
           (set-rear-ptr!3.22 queue new-pair)
           (queue 'print))
          (else
           (set-cdr! (rear-ptr3.22 queue) new-pair)
           (set-rear-ptr!3.22 queue new-pair)
           (queue 'print)))))

(define (delete-queue!3.22 queue)
  (cond ((empty-queue?3.22 queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr!3.22 queue (cdr (front-ptr3.22 queue)))
              (queue 'print))))

;(define q1 (make-queue3.22))
;(insert-queue!3.22 q1 'a)
;(insert-queue!3.22 q1 'b)
;(delete-queue!3.22 q1)
;(delete-queue!3.22 q1)

; EX 3.23
; show how to represent deques (double-ended queues) using pairs and give implementations of all operations
(define (make-deque) (cons '() '()))

(define (make-node data) (cons '() (cons `(,data) (cons '() '())))) ; see wikipedia entry for doubly linked lists
(define (prev-node node) (car node))
(define (data-field node) (car (cdr node)))
(define (next-node node) (car (cddr node)))
(define (set-prev-node! node new-node)
  (set-car! node new-node))
(define (set-next-node! node new-node)
  (set-car! (cddr node) new-node))

;(define test-node (make-node 'a))
;test-node

(define (front-ptr-deque deque) (car deque))
(define (rear-ptr-deque deque) (cdr deque))
(define (set-front-ptr-deque! deque item)
    (set-car! deque item))
(define (set-rear-ptr-deque! deque item)
  (set-cdr! deque item))

(define (print-deque deque) 
  (let ((front-deque (front-ptr-deque deque)))
    (define (actual-print-deque front-only-deque)
      (if (null? (next-node front-only-deque))
          (data-field front-only-deque)
          (cons (car (data-field front-only-deque)) (actual-print-deque (next-node front-only-deque)))))
    (actual-print-deque front-deque)))

(define (empty-deque? deque)
  (null? (front-ptr-deque deque)))

(define (front-deque deque)
  (if (empty-queue? deque)
      (error "FRONT called with an empty deque" deque)
      (data-field (front-ptr-deque deque))))

(define (rear-deque deque)
  (if (empty-queue? deque)
      (error "REAR called with an empty deque" deque)
      (data-field (rear-ptr-deque deque))))

(define (front-insert-deque! deque item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr-deque! deque new-node)
           (set-rear-ptr-deque! deque new-node)
           (print-deque deque))
          (else
           (set-prev-node! (front-ptr-deque deque) new-node)
           (set-next-node! new-node (front-ptr-deque deque))
           (set-front-ptr-deque! deque new-node)
           (print-deque deque)))))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr-deque! deque new-node)
           (set-rear-ptr-deque! deque new-node)
           (print-deque deque))
          (else
           (set-prev-node! new-node (rear-ptr-deque deque))
           (set-next-node! (rear-ptr-deque deque) new-node)
           (set-rear-ptr-deque! deque new-node)
           (print-deque deque)))))

(define (front-delete-deque! deque) 
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr-deque! deque (next-node (front-ptr-deque deque)))
         (set-prev-node! (front-ptr-deque deque) '())
         (print-deque deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr-deque! deque (prev-node (rear-ptr-deque deque)))
         (set-next-node! (rear-ptr-deque deque) '())
         (print-deque deque))))

; following the same representation model for queues, all operations work EXCEPT for rear-delete-deque. Need a way to store rear inserted items so that they can be retrieved
; in an ordering mirroring the order of the deque when following it from the front to the rear.

; the use of a doubly linked list is the solution
  
;(define d (make-deque))
;(front-insert-deque! d 'a)
;(rear-insert-deque! d 'y)
;(front-deque d)
;(front-delete-deque! d)
;(front-deque d)
;(rear-deque d)
;(rear-delete-deque! d)
;(rear-deque d)
;(print-deque d)

; Section 3.3.3 Representing Tables
; 1D Table:
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

; 2D Table:
(define (lookup2d key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert!2d key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; Creating Local Tables:
(define (make-local-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
(define (insert! key-1 key-2 value)
  (let ((subtable
         (assoc key-1 (cdr local-table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
  'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; make-local-table can be used to implement the get and put operations used in Section 2.4.3 for data-directed-programming:

(define operation-table (make-local-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; EX 3.24
; design a new make-table constructor that takes as an argument a same-key? procedure that will be used to test "equality" of keys. the new make-table should return a dispatch
; procedure that can be used to access appropriate lookup and insert! procedures for a local table
(define (make-table3.24 same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (same-key? key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; EX 3.25
; generalize 1 and 2d tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of
; keys. The lookup and insert! procedures should take as input a list of keys used to access the table

(define (new-table? table) (null? table))

(define (make-table3.25)
  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      
      (define (lookup-in-sub subtable inkeys)
        (if (null? (cdr inkeys))
            (let ((record
                   (assoc (car inkeys) subtable)))
              (if record (cdr record) false))
            (let ((subsubtable
                   (assoc (car inkeys) subtable)))
            (if subsubtable (lookup-in-sub (car (cdr subsubtable)) (cdr inkeys)) false))))
    
      (if (null? (cdr keys))
          (let ((record
                 (assoc (car keys) (cdr local-table))))
            (if record (cdr record) false))
          (let ((sub-table
                 (assoc (car keys) (cdr local-table))))
            (if sub-table (lookup-in-sub (car (cdr sub-table)) (cdr keys)) false))))

    (define (insert! keys value)

      (define (insert-in-sub! subtable keys)
        (if (null? (cdr keys))
            (let ((record
                   (assoc (car keys) subtable)))
              (if record
                  (set-cdr! record value)
                  (if (new-table? subtable)
                      (cons (cons (car keys) value) subtable)
                      (set-cdr! subtable
                                (cons (cons (car keys) value)
                                      (cdr subtable))))))
            (let ((subsubtable
                   (assoc (car keys) subtable)))
              (if subsubtable
                  (insert-in-sub! (car (cdr subsubtable)) (cdr keys))
                  (if (new-table? subtable)
                      (cons (list (car keys) 
                                  (insert-in-sub! '() (cdr keys))) subtable)
                      (set-cdr! subtable
                                (cons (list (car keys)
                                            (insert-in-sub! '() (cdr keys))) (cdr subtable))))))))
      
      (if (null? (cdr keys))
          (let ((record
                 (assoc (car keys) (cdr local-table))))
            (if record
                (set-cdr! record value)
                (set-cdr! local-table
                          (cons (cons (car keys) value)
                                (cdr local-table)))))
          (let ((sub-table
                 (assoc (car keys) (cdr local-table))))
           (if sub-table
               (insert-in-sub! (car (cdr sub-table)) (cdr keys))
               (set-cdr! local-table
                         (cons
                          (list (car keys) (insert-in-sub! '() (cdr keys)))
                          (cdr local-table))))))
    'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (lookup3.25 keys table)
  ((table 'lookup) keys))

(define (insert!3.25 keys value table)
  ((table 'insert!) keys value))

;(define test-table (make-table3.25))
;(insert!3.25 '(lvl1) 0 test-table)
;(lookup3.25 '(lvl1) test-table)
;(insert!3.25 '(lvl2 lvl3) 1 test-table)
;(insert!3.25 '(lvl2 lvl3.1 lvl4) 45 test-table)
;(lookup3.25 '(lvl2 lvl3) test-table)
;(lookup3.25 '(lvl2) test-table)
;(lookup3.25 '(lvl2 lvl3.1 lvl4) test-table)

; EX 3.26
; describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way
(define (lookup3.26 key table)
  (let ((record (assoc3.26 key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc3.26 key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc3.26 key (cdr records)))))

(define (insert!3.26 key value table)
  (define (insert-in-order tail-table front-table)
    (cond ((new-table? tail-table) (cons (cons key value) front-table))
          ((>= key (caar tail-table))
           (append front-table (cons (cons key value) tail-table)))
          (else (insert-in-order (cdr tail-table) (cons (car tail-table) front-table)))))
  (let ((record (assoc3.26 key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table ;;; TODO modify these 3 lines with insert-in-order function
                  (insert-in-order (cdr table) '()))))
  'ok)

; no change to make-table
;(define m (make-table))
;(insert!3.26 1 1 m)
;(insert!3.26 5 5 m)
;(insert!3.26 2 2 m)
;(insert!3.26 2 3 m)
;m
; redesigning insert! to place key-value entries in order in a table, which is what was done above, does not organize the table as a binary tree. reference file: 2.3.3pt2.rkt to
; refresh on the structure of a binary tree. a table can be structure in an analogous manner with key-value pairs in place of single data point entries

; EX 3.27
; memoization is a technique that enables a procedure to record, in a local table, values that have previously been computed. when the memoized procedure is asked to compute a value,
; it first checks the table to see if the value is already there and, if so, returns that value. otherwise, it computes the new value in the ordinary way and stores this in the table
(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
; draw an environment diagram to analyze the computation of (memo-fib 3). explain why memo-fib computes the nth fibonacci number in a number of steps proportional to n. would the
; scheme still work if we had simply defined memo-fib to be (memoize fib)

; diagram was hand-drawn
; memo-fib computes the nth fibonacci number in a number of steps proportional to n because once the algorithm reaches the base numbers (0 and 1) in n steps, each accumulating
; equation can use the table to substitute its constituent calls to memo-fib for the previously computed result
; the scheme would not have worked if we had simply defined memo-fib to be (memoize fib) because the subsequent calls to (fib (- n 1)) and (fib (- n 2)) would result in an exponential
; number of steps defeating the purpose of the tabulation. none of the accumulating steps of the procedure would have their results stored in the table either. 