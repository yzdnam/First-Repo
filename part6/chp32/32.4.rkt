;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |32.4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

; EX 508 - design split-structural
; EX 509 - design split, same as split-structural except using an accumulator... used accumulator for EX 508 naturally, remainder of solution to exercise in 32.4.1

; helper for split-structural 
; [List-of X] X -> [List-of X]
(define (add-to-end i loi)
  (cond
    [(empty? loi) (list i)]
    [else  (cons (first loi) (add-to-end i (rest loi)))]))
(check-expect (add-to-end "s" example-word)  (list "f" "a" "r" "t" "s"))

; helper for split-structural
; [NE-list-of X] -> [Ne-list-of X]
(define (all-but-last loi)
  (cond
    [(empty? (rest loi)) '()]
    [else (cons (first loi) (all-but-last (rest loi)))]))
(check-expect (all-but-last (list 1 2 3)) (list 1 2))

; [List-of 1String] N -> Editor
; the list of 1strings, ed,  represents the complete string in some editor and the number, x, represents the x-coord of a mouse click. the function produces (make-editor p s) such that:
; 1: p and s make up ed, and 2: x is larger than the image of p and smaller than the image of p extended with the first 1String on s (if any)
(define (split ed0 x)
  (local (; [List-of 1String] [List-of 1String] N -> Editor
          ; accumulator a is the 1Strings in ed0 preceding the contents of ed
          (define (split/a a ed)
            (cond
              [(zero? x) (make-editor '() ed)]
              [(>= x (image-width (editor-text ed0))) (make-editor ed0 '())]
              [else (if (<= (image-width (editor-text a)) x (image-width (editor-text (append a (list (first ed))))))
                        (make-editor a ed)
                        (split/a (add-to-end (first ed) a) (rest ed)))])))
    (split/a '() ed0)))

(define example-word (list "f" "a" "r" "t"))
(define example-width (image-width (editor-text example-word)))
(define example-split (split example-word 10))
(string=? (string-append (implode (editor-pre example-split)) (implode (editor-post example-split))) (implode example-word))
example-split

; EX 510 - design fmt
; N File Filename -> File
; reads all the words from the given file, arranges these words in the given order into lines of the given maximal width, and writes these lines to a file with the given filename
(define (fmt w in-f out-f)
  (local (; adds spaces between each word
          (define (add-spaces words)
            (cond
              [(empty? (rest words)) words]
              [else (cons (first words) (cons " " (add-spaces (rest words))))]))
          (define words-w-spaces (add-spaces (read-words in-f)))
          (define cntnts0 (reverse words-w-spaces))
          ; [List-of String] -> [List-of 1String]
          ; changes a list of strings into a list of 1strings so that the width of the line can be checked using the editor-text function
          (define (strings->1strings lostrings)
            (cond
              [(empty? lostrings) '()]
              [else (if (string-whitespace? (first lostrings))
                        (cons (first lostrings) (strings->1strings (rest lostrings)))
                        (append (explode (first lostrings)) (strings->1strings (rest lostrings))))]))
          ; [List-of String] [List-of [List-of String]] [List-of String] -> [List-of [List-of String]]
          ; separates the given list of strings into a list of lists of strings which represent lines that are no larger than the given maximal width
          ; accum-lines in fmt/a is the list of lists of strings in cntnts0 following the strings in cntnts
          ; accum-words in fmt/a is the list of strings in the line from cntnts0 after the strings in cntnts
          (define (fmt/a cntnts accum-lines accum-words)
            (local ((define cur-line-lgth (image-width (editor-text (strings->1strings accum-words)))))
              (cond
                [(and (empty? cntnts) (empty? accum-lines)) accum-words]
                [(empty? cntnts) (cons accum-words accum-lines)]
                [(> w cur-line-lgth) (fmt/a (rest cntnts) accum-lines (cons (first cntnts) accum-words))]
                [(equal? w cur-line-lgth) (fmt/a (rest cntnts) (cons accum-words accum-lines) '())]
                [else (fmt/a (cons (second accum-words) (cons (first accum-words) cntnts)) (cons (rest (rest accum-words)) accum-lines) '())])))
          (define words/line (fmt/a cntnts0 '() '()))
          ; [List-of [List-of String]] -> [List-of [List-of String]]
          ; adds "\n" to the end of each line
          (define (add\n lines)
            (local ((define (add\n-line line)
                      (reverse (cons "\n" (reverse line)))))
              (map add\n-line lines)))
          (define words/line-w-\n (add\n words/line))
          ; changes each Line from a list of separate strings into a list consisting of a single string
          (define (strings->string lostrings)
            (cond
              [(empty? (rest lostrings)) lostrings]
              [else (strings->string (cons (string-append (first lostrings) (second lostrings)) (rest (rest lostrings))))]))
          (define list-of-lines (map strings->string words/line-w-\n))
          ; [List-of [List-of String]] -> [List-of String]
          ; combines the list of lists of strings into a single list of strings
          (define (combine-list-of-lines lines)
            (cond
              [(empty? (rest lines)) (first lines)]
              [else (combine-list-of-lines (cons (append (first lines) (second lines)) (rest (rest lines))))]))
          (define one-list-of-lines (combine-list-of-lines list-of-lines))
          ; combines the list of strings into a list with one string
          (define product (strings->string one-list-of-lines)))
    (write-file out-f (first product))))
          


; [List-of 1String] -> [List-of 1String]
; returns the first 1strings in a given list up to the first " " 1string
(define (last-word-added lo1s0)
  (local ((define (last-word/a lo1s word)
            (cond
              [(empty? lo1s) '()]
              [(string=? " " (first lo1s)) word]
              [else (last-word/a (rest lo1s) (cons (first lo1s) word))])))
    (last-word/a lo1s0 '())))
(check-expect (last-word-added (list "o" "n" " " "i" "t")) (list "n" "o"))

; [List-of 1String] -> [List-of 1String]
; returns the 1strings in the given list except for the first 1strings up to and including the first space character
(define (wo-last-word lo1s0)
  (cond
    [(empty? lo1s0) '()]
    [(string=? (first lo1s0) " ") (rest lo1s0)]
    [else (wo-last-word (rest lo1s0))]))

(fmt 120 "ttt.txt" "tttnew.txt")