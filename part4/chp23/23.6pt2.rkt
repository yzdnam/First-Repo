;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23.6pt2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; [List-of DNA-symb] [List-of DNA-symb] -> Boolean
; first arg is the pattern, second arg is the search string
; returns true if the pattern is identical to the inital part of the search string; otherwise it returns #false
(define (DNAprefix pattern search-str)
  (cond
    [(empty? pattern) #true]
    [(empty? search-str) #false]
    [(equal? (first pattern) (first search-str)) (DNAprefix (rest pattern) (rest search-str))]
    [else #false]))

; returns first item in the search string beyond the pattern. If the lists are identical and there is no DNA letter beyond the pattern, the
; function signals an error
(define (DNAdelta pattern search-str)
  (cond
    [(empty? pattern) (first search-str)]
    [(equal? pattern search-str) (error "pattern and search string are identical.")]
    [(DNAprefix pattern search-str) (DNAdelta (rest pattern) (rest search-str))]))

(check-expect (DNAdelta (list 'a 'a 'a) (list 'a 'a 'a 'b)) 'b)

; EX 401
(define (sexp=? s1 s2)
  (cond
    [(and (number? s1) (number? s2)) (equal? s1 s2)]
    [(and (string? s1) (string? s2)) (string=? s1 s2)]
    [(and (symbol? s1) (symbol? s2)) (symbol=? s1 s2)]
    [(and (cons? s1) (cons? s2)) (equal-sl? s1 s2)]
    [else #false]))

(define (equal-sl? s1 s2)
  (cond
    [(and (empty? s1) (empty? s1)) #true]
    [(or (empty? s1) (empty? s2)) #false]
    [(sexp=? (first s1) (first s2)) (equal-sl? (rest s1) (rest s2))]
    [else #false]))

(check-expect (sexp=? (list 1 2 3) (list 1 2 3)) #true)