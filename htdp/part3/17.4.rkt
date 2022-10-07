;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |17.4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [Number Number -> Boolean] 
; -> [List-of Number]
; produces a version of alon0, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
 
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon 
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

(define (sorted>? ne-l)
  (cond
    [(empty? ne-l) (error ne-l" is an empty list. Aborting...")]
    [else (cond
            [(empty? (rest ne-l)) #true]
            [else (cond
                    [(> (first ne-l) (first (rest ne-l))) (sorted>? (rest ne-l))]
                    [else #false])])]))

; [X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; produces a function that determines whether 
; some list is sorted according to cmp
(define (sorted cmp)
  (lambda (lis) (equal? lis (sort lis cmp)))) 

(check-satisfied (sort-cmp '("c" "b") string<?)
                 (sorted string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <)
                 (sorted <))

(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp l)
  (equal? l (sort l cmp)))

; [X] [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))

(check-expect (part-of? '(2 3 4) '(1 2 3 4)) #true)
(check-expect (part-of? '(2 4) '(1 2 3 4)) #false)

(define (part-of? sl l)
  (cond
    [(and (empty? (rest sl)) (empty? (rest l)) (equal? (first sl) (first l))) #true]
    [(and (not (empty? (rest sl))) (empty? (rest l))) #false]
    [(equal? (first sl) (first l))
     (local (
             (define (part-of-rest? rest-sl rest-l)
               (cond
                 [(and (empty? (rest rest-sl)) (empty? (rest rest-l)) (equal? (first rest-sl) (first rest-l))) #true]
                 [(and (not (empty? (rest rest-sl))) (empty? (rest rest-l))) #false]
                 [(equal? (first rest-sl) (first rest-l)) (part-of-rest? (rest rest-sl) (rest rest-l))]
                 [(not (equal? (first rest-sl) (first rest-l))) #false])))
                 
       (part-of-rest? (rest sl) (rest l)))]
    [(not (equal? (first sl) (first l))) (part-of? sl (rest l))]))

; given a list and a sublist, returns the remainder of the list with
; the sublist removed
(define (pre-list sl l)
  (cond
    [(empty? (rest sl)) (reverse (rest (reverse l)))]
    [(equal? (first (reverse sl)) (first (reverse l))) (pre-list (reverse (rest (reverse sl))) (reverse (rest (reverse l))))]))

(check-expect (pre-list (list 3 4 5) (list 1 2 3 4 5)) (list 1 2))

(define (first-occurence-of-x? sl l)
  (not (member? (first sl) (pre-list sl l))))

(check-expect (first-occurence-of-x? (list 3 4 5) (list 1 2 3 4 5)) #true)
(check-expect (first-occurence-of-x? (list 3 4 5) (list 1 3 2 3 4 5)) #false)

; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; EX 293
; specification for find
; true if sl is the first sublist of l starting with the x given to find
(define (found? x l)
  (lambda (sl)
    (and (part-of? sl l)
         (equal? x (first sl))
         (first-occurence-of-x? sl l))))

(check-satisfied (find 3 (list 1 3 5 2 3 7)) (found? 3 (list 1 3 5 2 3 7)))

; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

(check-expect (index 3 '(1 2 3)) 2)

; EX 294
; specification for index
; true if n is the index of the first occurence of x in l
(define (is-index? x l)
  (lambda (n)
    (and (member? x l)
         (< n (length l))
         (equal? x (list-ref l n)))))

(check-satisfied (index 3 '(1 2 3)) (is-index? 3 '(1 2 3)))

    ; distances in terms of pixels 
    (define WIDTH 300)
    (define HEIGHT 300)
     
    ; N -> [List-of Posn]
    ; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
    (check-satisfied (random-posns 3)
                     (n-inside-playground? 3))
    (define (random-posns n)
      (build-list
        n
        (lambda (i)
          (make-posn (random WIDTH) (random HEIGHT)))))

; EX 295
; specification for random-posns
(define (n-inside-playground? n)
  (lambda (lop)
    (and (equal? (length lop) n)
         (andmap (lambda (pos) (and (< (posn-x pos) WIDTH)
                                    (>= (posn-x pos) 0)
                                    (>= (posn-y pos) 0)
                                    (< (posn-y pos) HEIGHT))) lop))))