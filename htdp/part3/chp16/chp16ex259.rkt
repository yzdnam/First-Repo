;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chp16ex259) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;(check-expect (string->word "fart") (list "f" "a" "r" "t"))
;(check-expect (arrangements (list "d" "e")) (list (list "d" "e") (list "e" "d")))
;(check-expect (words->strings EXAMPLE-LOW) EXAMPLE-LOS)

; String -> List-of-strings
; finds all words that the letters of some given word spell
(define (alternative-words s)
  (local (
           ; String -> Word
           ; converts s to the chosen word representation 
           (define string->word
             (explode s))
           
           
           ; Word -> List-of-words
           ; finds all rearrangements of word
           (define (arrangements w)
             (local (
                     ; 1String list-of-words -> list-of-words
                     ; produces a list-of-words with the 1string inserted at the beginning, between all letters, and at the end of all words
                     ; of the given list
                     (define (insert-everywhere/in-all-words 1str lows)
                       (local (
                               ; word -> list-of-words
                               ; produces a list of words with the 1string inserted at the beginning,
                               ; between all letters, and at the end of the given word
                               (define (insert-everywhere/word wrd)
                                 (local (
                                         ; word -> list-of-words
                                         ; produces a list of words with the given 1String appended and the first part of the word excised
                                         (define (add-letter wrd)
                                           (cond
                                             [(empty? wrd) (list (list 1str))]
                                             [else (cons (cons 1str wrd) (add-letter (rest wrd)))]))
                                         
                                         ; list-of-words -> list-of-words
                                         ; returns a list of words the same as the given word with a new letter inserted at
                                         ; the beginning, between all letters, and at the end of the given word
                                         (define (fix-words lows)
                                           (local (
                                                   ; word -> word
                                                   ; produces a word the same as the first given word with the exception of the first letter in the second given word
                                                   (define (return-to-word part-word)
                                                     (local (
                                                             ; word word -> word
                                                             ; returns the first half of a word given it's other half with a new letter appeneded 
                                                             (define (find-rest-word wrd part-word)
                                                               (cond
                                                                 [(or (empty? (reverse part-word))
                                                                      (not (equal? (first (reverse wrd)) (first (reverse part-word))))
                                                                      (empty? (rest (reverse part-word))))
                                                                      wrd]
                                                                 [else (find-rest-word (rest wrd) (rest part-word))])))
                                                             
                                                     (cond
                                                       [(empty? (rest part-word)) (append wrd part-word)]
                                                       [(equal? wrd (rest part-word)) part-word]
                                                       [(and (equal? (length (cons (first wrd) part-word)) (+ 1 (length wrd) ))
                                                             (equal? (rest wrd) (rest part-word)))
                                                        (cons (first wrd) part-word)]
                                                       [else (append (find-rest-word wrd part-word) part-word)]))))

                                                   
                                           (cond
                                             [(empty? (rest lows)) (list (return-to-word (first lows)))]
                                             [else (cons (return-to-word (first lows)) (fix-words (rest lows)))]))))
                                         
                                (fix-words (add-letter wrd)))))

                               
                       (cond
                         [(empty? lows) (list)]
                         [else (append (insert-everywhere/word (first lows)) (insert-everywhere/in-all-words 1str (rest lows)))]))))
                     
             (cond
               [(empty? w) (list '())]
               [else (create-set (insert-everywhere/in-all-words (first w) (arrangements (rest w))))])
               ))

; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (local (
          ; Word -> String
          ; converts w to a string
          (define (word->string w)
            (implode w)))
          
    (map word->string low)))


; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    [(empty? los) (list)]
    [else (cond
            [(member? (first los) AS-LIST) (cons (first los) (in-dictionary (rest los)))]
            [else (in-dictionary (rest los))])])))

  (in-dictionary
    (words->strings (arrangements string->word)))))

; list-of-strings -> list-of-strings
; returns a list with all duplicate strings eliminated from the original list (a set)
(define (create-set los)
  (cond
    [(empty? los) (list)]
    [else (cond
            [(member? (first los) (rest los)) (create-set (rest los))]
            [else (cons (first los) (create-set (rest los)))])]))

;------------------TESTS-----------

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define EXAMPLE-WORD (list "f" "a" "r" "t"))
(define EXAMPLE-LOW (list (list "f" "a" "r" "t") (list "b" "u" "r" "p")))
(define EXAMPLE-LOS (list "fart" "burp"))

;(check-expect (insert-everywhere/in-all-words "d"
;  (cons (list "e" "r")
;    (cons (list "r" "e")
;      '()))) (list (list "d" "e" "r") (list "e" "d" "r") (list "e" "r" "d") (list "d" "r" "e") (list "r" "d" "e") (list "r" "e" "d")))


;(check-expect (insert-everywhere/word "a" EXAMPLE-WORD) (list (list "a" "f" "a" "r" "t") (list "f" "a" "a" "r" "t")
;                                                              (list "f" "a" "a" "r" "t") (list "f" "a" "r" "a" "t") (list "f" "a" "r" "t" "a")))



;(check-expect (add-letter "a" EXAMPLE-WORD) (list (list "a" "f" "a" "r" "t") (list "a" "a" "r" "t") (list "a" "r" "t") (list "a" "t") (list "a")))

;(check-expect (return-to-word EXAMPLE-WORD (list "a" "r" "t")) (list "f" "a" "a" "r" "t"))

  
