;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |32|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))

(define (sum.v2 alon0)
  (local (; [List-of Number] Number -> Number
          ; computes the sum of the numbers on alon
          ; accumulator: a is the sum of the numbers that alon lacks from alon0
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                    (+ (first alon) a))])))
    (sum/a alon0 0)))

(check-expect (sum.v1 (list 1 2 3)) 6)
(check-expect (sum.v2 (list 1 2 3)) 6)

; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)
(check-expect (!.v2 3) 6)

(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n0)
  (local (; N ??? -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator a is the product of the natural numbers in the interval [n0, n).
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n)
                              (* n a))])))
    (!/a n0 1)))

; EX 497 measure how long it takes to evaluate (!.v1 20) 1000 times vs (!.v2 20)
(define (func-xtimes func x)
  (cond
    [(zero? x) func]
    [else (func-xtimes func (sub1 x))]))

(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

(define (height.v2 abt0)
  (local (; Tree ??? -> N
          ; measures the height of abt
          ; accumulator a is the number of steps it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
                (max (height/a (node-left abt)
                               (+ a 1))
                     (height/a (node-right abt)
                               (+ a 1)))])))
    (height/a abt0 0)))

; EX 498 - complete height.v3 which has two accumulators
(define (height.v3 abt0)
  (local (
          ; Tree N N -> N
          ; measures the height of abt
          ; accumulator s is the number of steps 
          ; it takes to reach abt from abt0
          ; accumulator m is the maximal height of
          ; the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) (max s m)]
              [else
                (max (h/a (node-left abt)
                          (add1 s) m)
                     (h/a (node-right abt)
                          (add1 s) (add1 m)))])))
          (h/a abt0 0 0)))

(define check-tree      
(make-node
  (make-node '()
             (make-node '() '()))
  '()))
(check-expect (height.v3 check-tree) 3)

; EX 499 - design an accumulator style product function which computes the product of a list of numbers
; [List-of Number] -> Number
; accumulator a is the product of the numbers in lon0 that precede the numbers in lon
(define (prod-acum lon0)
  (local ((define (prod/a lon a)
            (cond
              [(empty? lon) a]
              [else (prod/a (rest lon) (* (first lon) a))])))
    (prod/a lon0 1)))
(check-expect (prod-acum (list 1 2 3)) 6)

; EX 500 - design an accumulator style how-many function which computes the number of items on a list
; [List-of X] -> N
; accululator a is the number of items in loi0 that precede the items in loi
(define (hm-accum loi0)
  (local ((define (hm/a loi a)
            (cond
              [(empty? loi) a]
              [else (hm/a (rest loi) (+ 1 a))])))
    (hm/a loi0 0)))
(check-expect (hm-accum (list 1 2 3)) 3)
; compared to the regular version of how-many, the accumulator styled version will save stack space because applications of add1 will not be pending by the time the
; function reaches '()

; EX 501 - design an accululator-style add-to-pi which adds a natural number to pi without using +
; regular version:
; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))
; accumulator a is the difference between n0 and n added to pi
(check-within (a2p-accum 2) (+ 2 pi) 0.001)
(define (a2p-accum n0)
  (local ((define (a2p/a n a)
            (cond
              [(zero? n) a]
              [else (a2p/a (sub1 n) (add1 a))])))
    (a2p/a n0 pi)))

; EX 502 - design palindrome which accepts a NE-list and constructs a palindrome by mirroring the list around the last item using an accumulator
; a solution without an accumulator is below. it requires four traversals of the list. A version with an accumulator can compute the same result in a single traversal

; helper for palindrome
; [NE-list-of X] -> X
(define (last loi)
  (cond
    [(empty? (rest loi)) (first loi)]
    [else  (last (rest loi))]))

; helper for palindrome
; [NE-list-of X] -> [Ne-list-of X]
(define (all-but-last loi)
  (cond
    [(empty? (rest loi)) '()]
    [else (cons (first loi) (all-but-last (rest loi)))]))
(check-expect (all-but-last (list 1 2 3)) (list 1 2))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
  (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

; creates a palindrome from s0
; accumulator a is the letters from s0 preceding the letters in s in reverse order
(check-expect
  (mirror-accum (explode "abc")) (explode "abcba"))
(define (mirror-accum s0)
  (local ((define (mirror/a s a)
            (cond
              [(empty? (rest s)) (append s0 a)]
              [else (mirror/a (rest s) (cons (first s) a))])))
    (mirror/a s0 '())))
              
; EX 503
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate M)
  (cond
    [(andmap (lambda (x) (zero? (first x))) M) (error "Every equation has a leading coefficient of 0. SOE is unsolvable. Terminating.")]
    [(not (= (first (first M)) 0)) M]
    [else
     (rotate (append (rest M) (list (first M))))]))

(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate.v2 M0)
  (local (; Matrix Matrix -> Matrix 
          ; accumulator seen is the part of M0 that precedes M that has already been traversed by rotate/a
          (define (rotate/a M seen)
            (cond
              [(empty? M) (error "Every equation has a leading coefficient of 0. Terminating.")]
              [else (cond
                      [(not (zero? (first (first M)))) (append M seen)]
                      [else (rotate/a (rest M)
                                   (cons (first M) seen))
                     ])])))
    (rotate/a M0 '())))

; time a function on a matrix with X rows and the only row with a non-leading 0 being the last row
(define (time-rotate-func func x)
  (local (; creates matrix with only row with a non-leading 0 being the last row
          (define (create-mat x)
            (cond
              [(zero? x) (list (list 1 1 1))]
              [else (cons (list 0 1 1) (create-mat (sub1 x)))])))
    (time (func (create-mat x)))))

; EX 504 - design to10 which consumes a list of digits and produces the corresponding number using scientific notation
; example: '(1 0 2) produces (1*10^2 + 0*10^1 + 2*10^0)
; accumulator a is
(check-expect (to10-accum '(1 0 2)) 102)
(define (to10-accum lon0)
  (local ((define (to10/a lon a)
            (cond
              [(empty? lon) a]
              [else (to10/a (rest lon) (+ a (* (first lon) (expt 10 (length (rest lon))))))])))
    (to10/a lon0 0)))

(check-expect (to10 '(1 0 2)) 102)
(define (to10 lon)
  (cond
    [(empty? lon) 0]
    [else (+ (* (first lon) (expt 10 (length (rest lon)))) (to10 (rest lon)))]))

; time to10 functions
(define (time-to10-func func x)
  (local (; create list of numbers X numbers in length
          (define (create-lon x)
            (cond
              [(zero? x) '()]
              [else (cons (random 10) (create-lon (sub1 x)))])))
    (time (func (create-lon x)))))

; EX 505 - design is-prime which consumes a natural number and returns #true if it is prime and #false otherwise
; accumulator a is the number to be checked as the divisor to n
(check-expect (is-prime? 97) #true)
(check-expect (is-prime? 99) #false)
(define (is-prime? n)
  (local ((define (is-p/a n a)
           (cond
             [(equal? 1 a) #true]
             [else (if (> (gcd n a) 1) #false (is-p/a n (sub1 a)))])))
    (is-p/a n (floor (/ n 2)))))

; EX 506 - design an accumulator-style version of map
; accumulator a is the list of items in loi0 preceding loi that have already had the function applied to them
(define (map-accum func loi0)
  (local ((define (map/a func loi a)
            (cond
              [(empty? loi) a]
              [else (map/a func (rest loi) (cons (func (first loi)) a))])))
    (map/a func loi0 '())))

; EX 507 - using the signature of foldl as a guide, formulate the signature for fold/a and its accumulator invariant
; version 4
; [X Y] [X Y -> Y] Y [List-of X] -> Y
(define (f*ldl f e l0)
  (local (; Y [List-of X] -> Y
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    (fold/a e l0)))

; design build-l*st using an accumlator-style approach
(define (build-l*st n0 f)
  (local ((define (bl/a n a)
            (cond
              [(zero? n) (cons (f n) a)]
              [else (bl/a (sub1 n) (cons (f n) a))])))
    (bl/a (sub1 n0) '())))
(check-expect (build-l*st 22 add1) (build-list 22 add1))
(check-expect (build-l*st 3 (lambda (j) (+ j 3))) (build-list 3 (lambda (j) (+ j 3))))