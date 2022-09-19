;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |25|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

    (check-expect (bundle (explode "abcdefg") 3)
                  (list "abc" "def" "g"))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

; EX 422
; [List-of X] N -> [List-of [List-of X]
; returns a list of list chunks the size of n. each chunk representing a sub-sequence of items in the given list
(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else
     (cons (take l n) (list->chunks (drop l n) n))]))

(check-expect (list->chunks (list 1 2 3 4 5 6) 3) (list (list 1 2 3) (list 4 5 6)))

(define (new-bundle s n)
  (local ((define letters-bundled (list->chunks s n))
          (define (combine-lists lolists)
            (cond
              [(empty? lolists) '()]
              [else (cons (implode (first lolists)) (combine-lists (rest lolists)))])))
    (combine-lists letters-bundled)))

    (check-expect (new-bundle (explode "abcdefg") 3)
                  (list "abc" "def" "g"))

    (check-expect (new-bundle '("a" "b") 3) (list "ab"))

; EX 423
; String N -> [List-of String]
; produces a list of string chunks of size n
(define (partition s n)
  (cond
    [(empty? (explode s)) '()]
    [else (cons (substring s 0 n) (partition (substring s n (length (explode s))) n))]))

(check-expect (partition "nugget" 2) (list "nu" "gg" "et"))

; EX 424 on paper

; EX 425
; [List-of Number] Number -> [List-of Number]
; returns the numbers in the given list either smaller or larger than the given number
(define (return-half opr alon pivot)
  (cond
    [(empty? alon) '()]
    [else (local ((define first-alon (first alon)))
            (if (opr first-alon pivot) (cons first-alon (return-half opr (rest alon) pivot))
              (return-half opr (rest alon) pivot)))]))

(define (smallers alon pivot)
  (return-half < alon pivot))

(define (largers alon pivot)
  (return-half > alon pivot))

; EX 426
; Modify quick-sort< to take advantage of the fact that whenever quick-sort< consumes a list of one item, it returns it as is
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(equal? (length alon) 1) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; EX 427
; version of quick-sort< that uses sort< if the length of the input is below some threshold
(define THRESHOLD 3)
(define (quick-sort<.427 alon)
  (cond
    [(<= (length alon) THRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.427 (smallers alon pivot))
                    (list pivot)
                    (quick-sort<.427 (largers alon pivot))))]))

(check-expect (quick-sort<.427 (list 987 10 294 519 1010 29 11))
              (list 10 11 29 294 519 987 1010))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; EX 428
; the original quick-sort< returns a list that is strictly shorter than the input because smallers and largers will omit the pivot from the lists they return
; which in turn causes any duplicates of a pivot to be omitted from the output list
; the following version of quick-sort corrects this issue
(define (quick-sort<.428 alon)
  (cond
    [(empty? alon) '()]
    [(equal? (length alon) 1) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.428 (smallers alon pivot))
                    (accum-dupes alon pivot)
                    (quick-sort<.428 (largers alon pivot))))]))

(check-expect (quick-sort<.428 (list 3 3 1 2 5 8 2 2))
              (list 1 2 2 2 3 3 5 8))

; if n occurs in the given list, appends the occurence to the output list
(define (accum-dupes alon n)
  (cond
    [(empty? alon) '()]
    [else (local ((define first-alon (first alon)))
            (if (equal? first-alon n) (cons first-alon (accum-dupes (rest alon) n))
                (accum-dupes (rest alon) n)))]))

; EX 429
; use filter to define smallers and largers
(define (smallers-filter alon n)
  (filter (lambda (x) (< x n)) alon))

(check-expect (smallers-filter (list 1 2 3 4 5 6) 3)
              (list 1 2))

(define (largers-filter alon n)
  (filter (lambda (x) (> x n)) alon))

; EX 430
; variant of quick-sort< that uses only one comparison function. Its partitioning step divides the given list into a list that contains the items of alon on one side of the pivot
; and another one with those that are not on that side
(define (quick-sort.430 alon)
  alon)
  