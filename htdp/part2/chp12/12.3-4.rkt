;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3-4) (read-case-sensitive #t) (teachpacks ((lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; EX 212
; A List-of-words is one of:
;- '() or
;- (cons Word List-of-words)


(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))

(define EXAMPLE-WORD (list "f" "a" "r" "t"))
(define EXAMPLE-LOW (list (list "f" "a" "r" "t") (list "b" "u" "r" "p")))
(define EXAMPLE-LOS (list "fart" "burp"))

; EX 209
; String -> Word
; converts s to the chosen word representation 
(define (string->word s)
  (explode s))

(check-expect (string->word "fart") (list "f" "a" "r" "t"))

; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))

; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond
    [(empty? low) (list)]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))

(check-expect (words->strings EXAMPLE-LOW) EXAMPLE-LOS)


; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (create-set (insert-everywhere/in-all-words (first w) (arrangements (rest w))))]))

(check-expect (arrangements (list "d" "e")) (list (list "d" "e") (list "e" "d")))

; list-of-strings -> list-of-strings
; returns a list with all duplicate strings eliminated from the original list (a set)
(define (create-set los)
  (cond
    [(empty? los) (list)]
    [else (cond
            [(member? (first los) (rest los)) (create-set (rest los))]
            [else (cons (first los) (create-set (rest los)))])]))

; EX 213
; 1String list-of-words -> list-of-words
; produces a list-of-words with the 1string inserted at the beginning, between all letters, and at the end of all words
; of the given list
(define (insert-everywhere/in-all-words 1str lows)
  (cond
    [(empty? lows) (list)]
    [else (append (insert-everywhere/word 1str (first lows)) (insert-everywhere/in-all-words 1str (rest lows)))]))

(check-expect (insert-everywhere/in-all-words "d"
  (cons (list "e" "r")
    (cons (list "r" "e")
      '()))) (list (list "d" "e" "r") (list "e" "d" "r") (list "e" "r" "d") (list "d" "r" "e") (list "r" "d" "e") (list "r" "e" "d")))

; 1String word -> list-of-words
; produces a list of words with the 1string inserted at the beginning, between all letters, and at the end of the given word
(define (insert-everywhere/word 1str wrd)
   (fix-words wrd (add-letter 1str wrd)))

(check-expect (insert-everywhere/word "a" EXAMPLE-WORD) (list (list "a" "f" "a" "r" "t") (list "f" "a" "a" "r" "t")
                                                              (list "f" "a" "a" "r" "t") (list "f" "a" "r" "a" "t") (list "f" "a" "r" "t" "a")))

; word list-of-words -> list-of-words
; returns a list of words the same as the given word with a new letter inserted at the beginning, between all letters, and at the end of the given word
(define (fix-words wrd lows)
  (cond
    [(empty? (rest lows)) (list (return-to-word wrd (first lows)))]
    [else (cons (return-to-word wrd (first lows)) (fix-words wrd (rest lows)))]))

; 1String word -> list-of-words
; produces a list of words with the given 1String appended and the first part of the word excised
(define (add-letter letter wrd)
  (cond
    [(empty? wrd) (list (list letter))]
    [else (cons (cons letter wrd) (add-letter letter (rest wrd)))]))

(check-expect (add-letter "a" EXAMPLE-WORD) (list (list "a" "f" "a" "r" "t") (list "a" "a" "r" "t") (list "a" "r" "t") (list "a" "t") (list "a")))

; word word -> word
; produces a word the same as the first given word with the exception of the first letter in the second given word
(define (return-to-word orig-word part-word)
  (cond
    [(empty? (rest part-word)) (append orig-word part-word)]
    [(equal? orig-word (rest part-word)) part-word]
    [(and (equal? (length (cons (first orig-word) part-word)) (+ 1 (length orig-word) ))
          (equal? (rest orig-word) (rest part-word)))
     (cons (first orig-word) part-word)]
    [else (append (find-rest-word (reverse orig-word) (reverse part-word)) part-word)]))

(check-expect (return-to-word EXAMPLE-WORD (list "a" "r" "t")) (list "f" "a" "a" "r" "t"))

; word word -> word
; returns the first half of a word given it's other half with a new letter appeneded 
(define (find-rest-word orig-word part-word)
  (cond
    [(empty? part-word) (reverse orig-word)]
    [(not (equal? (first orig-word) (first part-word))) (reverse orig-word)]
    [(empty? (rest part-word)) (reverse orig-word)]
    [else (find-rest-word (rest orig-word) (rest part-word))]))
  
; EX 211
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    [(empty? los) (list)]
    [else (cond
            [(member? (first los) AS-LIST) (cons (first los) (in-dictionary (rest los)))]
            [else (in-dictionary (rest los))])]))
