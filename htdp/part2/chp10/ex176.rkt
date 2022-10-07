;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex176) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define row11 (cons 11 (cons 12 (cons 13 '()))))
(define row21 (cons 21 (cons 22 (cons 23 '()))))
(define row31 (cons 31 (cons 32 (cons 33 '()))))
(define mat11 (cons row11 (cons row21 (cons row31 '()))))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(define wor11 (cons 11 (cons 21 (cons 31 '()))))
(define wor21 (cons 12 (cons 22 (cons 32 '()))))
(define wor31 (cons 13 (cons 23 (cons 33 '()))))
(define tam11 (cons wor11 (cons wor21 (cons wor31 '()))))

(check-expect (transpose mat1) tam1)
(check-expect (transpose mat11) tam11)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; matrix -> list-of-numbers
; consumes a matrix and produces the first column
; as a list of numbers

(check-expect (first* mat1) wor1)

(define (first* matrix)
  (cond
    [(empty? matrix) '()]
    [else (cons (first (first matrix)) (first* (rest matrix)))]))

; matrix -> matrix
; returns a matrix with the the first column removed from the
; original matrix

(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 22 '()) '())))

(define (rest* matrix)
  (cond
    [(empty? matrix) '()]
    [else (cons (rest (first matrix)) (rest* (rest matrix)))]))