;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32.4.workspace) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

          (define (add-spaces words)
            (cond
              [(empty? (rest words)) words]
              [else (cons (first words) (cons " " (add-spaces (rest words))))]))
          
          (define words-w-spaces (add-spaces (read-words "ttt.txt")))
          (define cntnts0 (reverse words-w-spaces))


          (define (strings->1strings lostrings)
            (cond
              [(empty? lostrings) '()]
              [else (if (string-whitespace? (first lostrings))
                        (cons (first lostrings) (strings->1strings (rest lostrings)))
                        (append (explode (first lostrings)) (strings->1strings (rest lostrings))))]))

          (define (fmt/a w cntnts accum-lines accum-words)
            (local ((define cur-line-lgth (image-width (editor-text (strings->1strings accum-words)))))
              (cond
                [(and (empty? cntnts) (empty? accum-lines)) accum-words]
                [(empty? cntnts) (cons accum-words accum-lines)]
                [(> w cur-line-lgth) (fmt/a w (rest cntnts) accum-lines (cons (first cntnts) accum-words))]
                [(equal? w cur-line-lgth) (fmt/a w (rest cntnts) (cons accum-words accum-lines) '())]
                [else (fmt/a w (cons (first accum-words) cntnts) (cons (rest accum-words) accum-lines) '())])))

          (fmt/a 70 (list "fart" " " "face" " " "homie" " " "yo") '() '())

                    (define words/line (fmt/a 120 cntnts0 '() '()))