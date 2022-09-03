;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; EX 195
; Letter Dictionary -> Number
; counts how many words in the given dictionary start with the given letter
(define (starts-with# l dict)
  (cond
    [(empty? dict) 0]
    [else (+ (check-word l (first dict))
             (starts-with# l (rest dict)))]))

(check-expect (starts-with# "a" (list "aardvark" "clue" "balls" "blue" "animal")) 2)

; Letter String -> Number
; returns 1 if given string starts with the given letter, 0 if it doesn't
(define (check-word l str)
  (cond
    [(equal? l (first (explode str))) 1]
    [else 0]))

(check-expect (check-word "a" "aardvark") 1)
(check-expect (check-word "b" "aardvark") 0)

; EX 196

(define-struct letter-count [letter count])
; A letter-count is a structure composed of a letter and a number

; dictionary -> List-of-letter-counts
; consumes a dictionary and counts how often each letter is used as the first one of a word in the given dictionary
(define (count-by-letter dict)
  (create-lcs LETTERS dict))

; list-of-letters dictionary -> list-of-letter-counts
; consumes a string, creates a letter-count or adds one to an existing one
(define (create-lcs lols dict)
  (cond
    [(empty? lols) '()]
    [else (cons (make-letter-count (first lols) (starts-with# (first lols) dict)) (create-lcs (rest lols) dict))]))

; EX 197
; dictionary -> letter-count
; returns the letter-count for the letter that occurs most often as the first one in the given dictionary
(define (most-frequent dict)
  (first (sort>-lcs (count-by-letter dict))))

; list-of-letter-counts -> list-of-letter-counts
; sorts a list of letter counts by their count component in descending order
(define (sort>-lcs lolcs)
  (cond
    [(empty? lolcs) '()]
    [else (insert-lc (first lolcs) (sort>-lcs (rest lolcs)))]))

; letter-count list-of-letter-counts -> list-of-letter-counts
; compares the count component of the given letter-count with the first letter-count's in the given list
; inserts the given letter-count in the first spot in the list if its count component is greater
; inserts the given letter-count behind the first letter-count in the given list if not
(define (insert-lc lc lolcs)
  (cond
    [(empty? lolcs) (cons lc lolcs)]
    [(> (letter-count-count lc) (letter-count-count (first lolcs)))
     (cons lc lolcs)]
    [else (cons (first lolcs) (insert-lc lc (rest lolcs)))]))

(check-expect (sort>-lcs (cons (make-letter-count "a" 66) (cons (make-letter-count "b" 100) (cons (make-letter-count "c" 99) '()))))
              (cons (make-letter-count "b" 100) (cons (make-letter-count "c" 99) (cons (make-letter-count "a" 66) '()))))

; EX 197 ALT
; dictionary -> letter-count
; returns letter-count for letter that occurs most often as the first letter to words in the given dictionary
; by picking the pair with the maximum count
(define (most-frequent-alt dict)
  (choose-max (first (count-by-letter dict)) (count-by-letter dict)))

; letter-count list-of-letter-counts -> letter-count
; returns the given letter-count if it is larger than all the letter-counts in the given list
; or the maximum letter-count in the given list if it is larger
(define (choose-max lc lolcs)
  (cond
    [(empty? (rest lolcs)) lc]
    [else (cond
            [(> (letter-count-count lc) (letter-count-count(first lolcs))) (choose-max lc (rest lolcs))]
            [else (choose-max (first lolcs) (rest lolcs))])]))

; EX 198
; dictionary -> list-of-dictionarys
; consumes a dictionary and produces a list of dictionarys, one per letter
(define (words-by-first-letter dict)
  (remove-empties (create-list-of-dicts LETTERS dict))) 

; list-of-lists -> list-of-lists
; removes empty lists from a list-of-lists
(define (remove-empties lols)
  (cond
    [(empty? lols) (list)]
    [else (cond
            [(not (empty? (first lols))) (cons (first lols) (remove-empties (rest lols)))]
            [else (remove-empties (rest lols))])]))

(check-expect (words-by-first-letter (list "a" "aa" "b")) (list (list "a" "aa") (list "b")))

; list-of-letters dictionary -> list-of-dictionarys
; creates a list of dictionaries from the given dictionary starting with a letter for each letter found
; in the given list of letters
(define (create-list-of-dicts lols dict)
  (cond
    [(empty? lols) (list)]
    [else (cons (create-first-letter-list (first lols) dict) (create-list-of-dicts (rest lols) dict))]))

; letter dictionary -> dictionary
(define (create-first-letter-list letter dict)
  (cond
    [(empty? dict) (list)]
    [else (cond
            [(equal? letter (first (explode (first dict))))
             (cons (first dict) (create-first-letter-list letter (rest dict)))]
            [else (create-first-letter-list letter (rest dict))])]))

; most-frequent using words-by-first-letter
; dictionary -> letter-count
; returns the letter-count for the letter that occurs most often as the first one in the given dictionary
; using words-by-first-letter
(define (most-frequent.v2 dict)
  (make-lc-4-same1st-letter-dict (choose-max-dict (first (words-by-first-letter dict)) (words-by-first-letter dict))))

(check-expect (equal? (most-frequent.v2 AS-LIST) (most-frequent AS-LIST)) #true)

; list-of-dicts -> dict
; returns dict with most entries from a list of dicts or the given dict if it is larger than all dicts in the given list
(define (choose-max-dict dict lods)
  (cond
    [(empty? (rest lods)) dict]
    [else (cond
            [(> (length dict) (length (first lods))) (choose-max-dict dict (rest lods))]
            [else (choose-max-dict (first lods) (rest lods))])]))

; dict -> letter-count
; given a dict with strings that all share the same first letter, return the letter count representing the dict
(define (make-lc-4-same1st-letter-dict dict)
  (make-letter-count (first (explode (first dict))) (length dict)))