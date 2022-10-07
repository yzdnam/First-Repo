;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp10.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A list-of-strings is one of:
; '()
; (cons string list-of-strings)

; A list-of-list-of-strings is one of:
; '()
; (cons list-of-strings list-of-list-of-strings)

; EX 172
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

; EX 173
; file -> file
; consumes the name of a file
; sends to "checked-list-of-lists" to read the file and send to
; "article-check" to remove the articles
; writes the result out to a file whose name is the result
; of concatenating "no-articles-" with the original name
; of the file
(define (remove-articles-main n)
  (write-file (string-append "no-articles-" n) (collapse (checked-list-of-lists (readit n)))))

(define (readit n)
  (read-words/line n))

; list-of-list-of-strings -> list-of-lists-of-strings (lolos)
; sends each line to "article-check" to remove the articles
; sends to main to write the new file

(check-expect (checked-list-of-lists (cons (cons "a" (cons "b" (cons "c" '()))) (cons (cons "d" (cons "e" (cons "f" '())))'())))
              (cons (cons "b" (cons "c" '())) (cons (cons "d" (cons "e" (cons "f" '())))'())))

(define (checked-list-of-lists n)
  (cond
    [(empty? n) '()]
    [else (cons (article-check (first n))
                (checked-list-of-lists (rest n)))]))


; list-of-strings -> list-of-strings
; consumes a list of strings received from "checked-list-of-lists"
; removes articles from the list and returns the new list of strings

(check-expect (article-check (cons "fuck" (cons "a" (cons "you" (cons "an" '()))))) (cons "fuck" (cons "you" '())))

(define (article-check los)
  (cond
    [(empty? los) '()]
    [else (cond
            [(or (equal? (first los) "a")
                 (equal? (first los) "an")
                 (equal? (first los) "the"))
             (article-check (rest los))]
            [else (cons (first los) (article-check (rest los)))])]))
