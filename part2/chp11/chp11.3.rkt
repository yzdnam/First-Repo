;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname chp11.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EX 145
; NEList-of-temperatures -> Boolean
; Produces #true if the temperatures are listed in descending order
; (1st>2nd>3rd...). Otherwise, it produces #false
(define (sorted>? ne-l)
  (cond
    [(empty? ne-l) (error ne-l" is an empty list. Aborting...")]
    [else (cond
            [(empty? (rest ne-l)) #true]
            [else (cond
                    [(> (first ne-l) (first (rest ne-l))) (sorted>? (rest ne-l))]
                    [else #false])])]))

; List-of-numbers -> List-of-numbers 
; rearranges alon in descending order
 
(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)
              
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-satisfied (sort>/bad (list 1 3 5)) sorted>?)

(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

; EX 187
 (define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points
; design a program that sorts lists of game players by score

; list-of-gps -> list-of-gps
; sorts list-of-gps in descending order by score
(define (sort-gps logps)
  (cond
    [(empty? logps) '()]
    [else (insert-gp (first logps) (sort-gps (rest logps)))]))

; gp list-of-gps -> list-of-gps
; inserts gp into a sorted list of gps
(define (insert-gp gp logps)
  (cond
    [(empty? logps) (cons gp '())]
    [else (if (>= (gp-score gp) (gp-score (first logps)))
            (cons gp logps)
            (cons (first logps) (insert-gp gp (rest logps))))]))

; list-of-gps -> Boolean
; Produces #true if the gps are listed in descending order
; (1st>2nd>3rd...). Otherwise, it produces #false
(define (sortedgps>? ne-l)
  (cond
    [(empty? ne-l) (error ne-l" is an empty list. Aborting...")]
    [else (cond
            [(empty? (rest ne-l)) #true]
            [else (cond
                    [(> (gp-score (first ne-l)) (gp-score (first (rest ne-l)))) (sortedgps>? (rest ne-l))]
                    [else #false])])]))

(check-satisfied (sort-gps (list (make-gp "a" 1) (make-gp "b" 5) (make-gp "c" 10))) sortedgps>?)

; EX 188
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time
; design a program that sorts lists of emails by date

; list-of-emails -> list-of-emails
; sorts list of emails by date in descending orderf
(define (sort-mail lom)
  (cond
    [(empty? lom) '()]
    [else (insert-mail (first lom) (sort-mail (rest lom)))]))

; mail list-of-mail -> list-of-mail
; inserts mail into a sorted list of mail
; based on date sent
(define (insert-mail mail lom)
  (cond
    [(empty? lom) (cons mail '())]
    [else (if (>= (email-date mail) (email-date (first lom)))
              (cons mail lom)
              (cons (first lom) (insert-mail mail (rest lom))))]))

; list-of-mail -> Boolean
; Produces #true if list of mail is listed in descending order
; otherwise, produces #false
(define (sortedmail>? lom)
  (cond
    [(empty? lom) (error lom" is an empty list. Aborting...")]
    [else (cond
            [(empty? (rest lom)) #true]
            [else (cond
                    [(> (email-date (first lom)) (email-date (first (rest lom)))) (sortedmail>? (rest lom))]
                    [else #false])])]))

(check-satisfied (sort-mail (list (make-email "mark" 240 "whattup") (make-email "foo" 1040 "faggot") (make-email "lemon" 2000 "hoe"))) sortedmail>?)

; EX 189
; Number sorted-list-of-numbers -> Boolean
; determines whether some number occurs in a list of numbers
; takes advantage of the fact that the list is sorted
(define (search-sorted n aslon)
  (cond
    [(empty? aslon) #false]
    [(not (sorted>? aslon)) (error "given list must be sorted")]
    [(< (first aslon) n) #false]
    [else (or (= (first aslon) n)
              (search-sorted n (rest aslon)))]))

(check-error (search-sorted 5 (list 1 2 3)) "given list must be sorted")
(check-expect (search-sorted 8 (list 5 4 3 2)) #false)
(check-expect (search-sorted 8 (list 11 10 9)) #false)
(check-expect (search-sorted 1 (list 4 3 2)) #false)
(check-expect (search-sorted 8 (list 8 5 4 3 2)) #true)
(check-expect (search-sorted 8 (list 11 10 9 8)) #true)
(check-expect (search-sorted 1 (list 4 3 2 1)) #true)

; EX 190
; list-of-1strings -> list-of-lists-of-1strings
; produces a list of lists of all prefixes to the given list
(define (prefixes lo1)
  (cond
    [(empty? lo1) lo1]
    [else (cons lo1 (prefixes (reverse (rest (reverse lo1)))))]))

(check-expect (prefixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(check-expect (prefixes (list "a" "b" "c" "d")) (list (list "a" "b" "c" "d") (list "a" "b" "c") (list "a" "b") (list "a") ))
(check-expect (prefixes (list "a" "b" "c" "d" "e")) (list (list "a" "b" "c" "d" "e") (list "a" "b" "c" "d") (list "a" "b" "c") (list "a" "b") (list "a") ))

; list-of-1strings -> list-of-lists-of-1strings
; produces a list of lists of all suffixes to the given list
(define (suffixes lo1)
  (cond
    [(empty? lo1) lo1]
    [else (cons lo1 (suffixes (rest lo1)))]))

(check-expect (suffixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c") ))
(check-expect (suffixes (list "a" "b" "c" "d")) (list (list "a" "b" "c" "d") (list "b" "c" "d") (list "c" "d") (list "d") ))
(check-expect (suffixes (list "a" "b" "c" "d" "e")) (list (list "a" "b" "c" "d" "e") (list "b" "c" "d" "e") (list "c" "d" "e") (list "d" "e") (list "e") ))