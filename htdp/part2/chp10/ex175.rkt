;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

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
(define-struct wcount [1strings words lines])
; file -> stdout
; counts the number of 1strings, words, and lines in a given file
; and prints to stdout
(define (wc infile)
  (present (count (read-1strings infile))))

; wcount -> string
; presents a wcount as a string
; identifying each of the components
; of the structure
(check-expect (present (make-wcount 1 1 1)) "1strings: 1\nwords: 1\nlines: 1")

(define (present counter)
  (format "1strings: ~x~%words: ~a~%lines: ~a" (wcount-1strings counter) (wcount-words counter) (wcount-lines counter)))

;iterates through the lines in the file
(define (count 1strings)
  (cond
    [(empty? 1strings) (make-wcount 0 0 0)]
    [else (make-wcount (count-1strings 1strings)
                       (count-words 1strings)
                       (count-lines 1strings)
                )]))

; counts 1strings
; list-of-1strings -> number
(define (count-1strings lo1s)
  (cond
    [(empty? lo1s) 0]
    [else (+ 1 (count-1strings (rest lo1s)))]))

; counts words
; list-of-1strings -> number
(define (count-words lo1s)
  (cond
    [(empty? lo1s) 0]
    [else (if (equal? (first lo1s) " ")
              (+ 1 (count-words (rest lo1s)))
              (count-words (rest lo1s)))]))

; counts lines
; list-of-1strings -> number
(define (count-lines lo1s)
  (cond
    [(empty? lo1s) 0]
    [else (if (equal? (first lo1s) "\n")
              (+ 1 (count-lines (rest lo1s)))
              (count-lines (rest lo1s)))]))