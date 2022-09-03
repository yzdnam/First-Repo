;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; 1String -> String
; converts the given 1String to a 3-letter numeric String
         
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
         
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
        (string-append "00" (code1 s))]
    [(< (string->int s) 100)
        (string-append "0" (code1 s))]))
         
; 1String -> String
; converts the given 1String into a String
         
(check-expect (code1 "z") "122")
         
(define (code1 c)
  (number->string (string->int c)))

; list-of-list-of-strings -> string
; covnerts a list of lines into a string
; with blank spaces separating strings and newlines
; separating lines
(define (collapse lln)
  (cond
    [(empty? lln) ""]
    [else (string-append (string-convert (first lln)) "\n" (collapse (rest lln)))]))

; list-of-strings -> string
; converts a list of strings into a single string
; with blank spaces separating strings
(define (string-convert los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los) " " (string-convert (rest los)))]))

; EX 174
; file -> stdout
; encodes text files numerically and prints to stdout
(define (encode infile)
  (collapse (convert-lolos (read-words/line infile))))

;iterates through the lines in the file
(define (convert-lolos lolos)
  (cond
    [(empty? lolos) '()]
    [else (cons (convert-list-of-strings (first lolos))
                (convert-lolos (rest lolos)))]))

;iterates through the strings in the line
(define (convert-list-of-strings los)
  (cond
    [(empty? los) '()]
    [else (cons (convert-string (first los))
                (convert-list-of-strings (rest los)))]))

;converts word to a list of letters 
(define (convert-string word)
  (cond
    [(empty? word) ""]
    [else (convert-letters (explode word))]))

;encodes list of letters and converts the encoding to a string
(define (convert-letters lols)
  (cond
    [(empty? lols) ""]
    [else (string-append (encode-letter (first lols))
                (convert-letters (rest lols)))]))
                         
