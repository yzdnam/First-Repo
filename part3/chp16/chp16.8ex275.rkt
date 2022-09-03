;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chp16.8ex275) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

(define (sort-by-x> x item1 item2)
  (> (x item1) (x item2)))

; String -> boolean
; returns true if given string starts with the given letter, false if it doesn't
(define (start-with? l str)
    (equal? l (first (explode str))))

; list -> boolean
(define (not-empty? l)
 (not (empty? l)))

; EX 195
; Letter Dictionary -> Number
; counts how many words in the given dictionary start with the given letter
(define (starts-with# l dict)
  (local (
          ; number  -> number
          (define (add-check-word str number)
            (cond
              [(start-with? l str) (+ 1 number)]
              [else number])))
    (foldr add-check-word 0 dict)))

(check-expect (starts-with# "a" (list "aardvark" "clue" "balls" "blue" "animal")) 2)

; EX 196

(define-struct letter-count [letter count])
; A letter-count is a structure composed of a letter and a number

; EX 197
; dictionary -> letter-count
; returns the letter-count for the letter that occurs most often as the first one in the given dictionary
(define (most-frequent dict)
  (local (
          (define count-by-letter
            (local (;letter -> letter-count
                    (define (create-lcs-abs letter)
                      (make-letter-count letter (starts-with# letter dict))))
              (map create-lcs-abs LETTERS)))

          ; letter-count letter-count -> boolean
          (define (count>? a b)
            (sort-by-x> letter-count-count a b)))
    
  (first (sort count-by-letter count>?))))

(check-expect (most-frequent (list "a" "ab" "ac" "d" "e" "q")) (make-letter-count "a" 3))

; EX 198
; dictionary -> list-of-dictionarys
; consumes a dictionary and produces a list of dictionarys, one per letter
(define (words-by-first-letter dict)
  (local (
          ; list-of-letters dictionary -> list-of-dictionarys
          ; creates a list of dictionaries from the given dictionary starting with a letter for each letter found
          ; in the given list of letters
          (define (create-list-of-dicts lols dict)
            (local (
                    ; letter -> dictionary
                    (define (create-letter-list letter)
                      (local (
                              ; string -> boolean
                              (define (start-with-specific? str)
                                (start-with? letter str)))
                        (filter start-with-specific? dict))))
             (map create-letter-list lols))))
    (filter not-empty? (create-list-of-dicts LETTERS dict)))) 

(check-expect (words-by-first-letter (list "a" "aa" "b")) (list (list "a" "aa") (list "b")))

; most-frequent using words-by-first-letter
; dictionary -> letter-count
; returns the letter-count for the letter that occurs most often as the first one in the given dictionary
; using words-by-first-letter
(define (most-frequent.v2 dict)
  (local (
          (define (sort-by-len l1 l2)
            (sort-by-x> length l1 l2))
          (define lolowrds (words-by-first-letter dict))
          (define sorted-lolowrds (sort lolowrds sort-by-len))
          (define most-freq-let (first sorted-lolowrds)))
    (make-letter-count (first (explode (first most-freq-let))) (length most-freq-let))))

(check-expect (equal? (most-frequent.v2 AS-LIST) (most-frequent AS-LIST)) #true)