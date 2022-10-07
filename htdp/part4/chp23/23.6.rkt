;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23.6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
; EX 396
(define LETTERS (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "'"))
; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word Letter -> HM-Word
; consumes the word to be guessed, a word that represents how much/little the guessing player has discovered,
; and the current guess
(define (compare-word the-word current-status guess)
  (cond
    [(or (empty? the-word) (empty? current-status)) '()]
    [else (if (equal? (first the-word) guess)
              (cons guess (compare-word (rest the-word) (rest current-status) guess))
              (cons (first current-status) (compare-word (rest the-word) (rest current-status) guess)))]))

(check-expect (compare-word (list "a" "b" "c") (list "a" "_" "_") "b") (list "a" "b" "_"))

; EX 397
(define-struct employee-structure [name ssn rate])
(define-struct work-record [name hours])
(define-struct employee-wage [name wage])
; [List-of employee-structure] [List-of work-record] -> [List-of employee-wage]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length
(check-expect (wages*.v3 (list (make-employee-structure "charles" 001 10) (make-employee-structure "shane" 002 11))
                         (list (make-work-record "charles" 2) (make-work-record "shane" 4)))
              (list (make-employee-wage "charles" 20) (make-employee-wage "shane" 44)))
(check-expect (wages*.v3 (list (make-employee-structure "chuck" 001 5.65)) (list (make-work-record "chuck" 40)))
              (list (make-employee-wage "chuck" 226.0)))
(check-error (wages*.v3 (list (make-employee-structure "charles" 001 10) (make-employee-structure "shane" 002 11) (make-employee-structure "mark" 003 4))
                         (list (make-work-record "charles" 2) (make-work-record "shane" 4)))
              "cannot find timecard for employee")
(check-error (wages*.v3 (list (make-employee-structure "chuck" 001 5.65)) (list (make-work-record "chuck" 40) (make-work-record "fuck" 10)))
              "cannot find employee for timecard")

(define (wages*.v3 lostructs lowrecs)
  (cond
    [(and (empty? lostructs) (empty? lowrecs)) '()]
    [(empty? lowrecs) (error "cannot find timecard for employee")]
    [(empty? lostructs) (error "cannot find employee for timecard")]
    [else (if (equal? (make-wage-w-struct (first lostructs) lowrecs) (make-wage-w-wrec (first lowrecs) lostructs))
              (cons (make-wage-w-struct (first lostructs) lowrecs) (wages*.v3 (rest lostructs) (rest lowrecs)))
              (cons (make-wage-w-struct (first lostructs) lowrecs) (cons (make-wage-w-wrec (first lowrecs) lostructs) (wages*.v3 (rest lostructs) (rest lowrecs)))))]))

; employee-structure [List-of work-record] -> employee-wage
(define (make-wage-w-struct struct lowrecs)
  (cond
    [(empty? lowrecs) (error "cannot find timecard for employee")]
    [(equal? (employee-structure-name struct) (work-record-name (first lowrecs)))
     (weekly-wage struct (first lowrecs))]
    [else (make-wage-w-struct struct (rest lowrecs))]))

; work-record [List-of employee-structure] -> employee-wage
(define (make-wage-w-wrec wrec lostructs)
  (cond
    [(empty? lostructs) (error "cannot find employee for timecard")]
    [(equal? (work-record-name wrec) (employee-structure-name (first lostructs)))
     (weekly-wage (first lostructs) wrec)]
    [else (make-wage-w-wrec wrec (rest lostructs))]))

; employee-structure work-record -> employee-wage
; computes the weekly wage from pay-rate and hours
(define (weekly-wage struct wrkrec)
  (make-employee-wage (employee-structure-name struct) (* (employee-structure-rate struct) (work-record-hours wrkrec))))

; EX 398
; [List-of Number] [List-of Number] -> Number
; consumes two equally long lists: a linear combination and a list of variable values. Produces the value of the combination for these values
(define (value combos varvals)
  (cond
    [(and (empty? combos) (empty? varvals)) 0]
    [else (+ (* (first combos) (first varvals)) (value (rest combos) (rest varvals)))]))

(check-expect (value (list 1 2 3 4 5) (list 1 1 1 1 1)) 15)

; EX 399
(define SISTERS (list "Louise" "Jane" "Laura" "Dana" "Mary"))

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (list-pick l (random (my-length l))))

(define (my-length l)
  (foldr (lambda (x y) (+ 1 y)) 0 l))

(check-expect (my-length (list 100 100)) 2)
(check-expect (my-length (list 100 100 1 1 1 )) 5)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
   (cond
    [(empty? l)
     (error "list too short")]
    [(= n 0) (first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [(compare-two-lists names (first ll)) (cons (first ll) (non-same names (rest ll)))]
    [else (non-same names (rest ll))]))

(check-expect (non-same (list "Louise" "Jane") (list (list "Louise" "Jane") (list "Jane" "Louise"))) (list (list "Jane" "Louise")))

; false if the lists have an identical element in the same spot, true otherwise
(define (compare-two-lists names li)
  (cond
    [(and (empty? names) (empty? li)) #true]
    [(equal? (first names) (first li)) #false]
    [else (compare-two-lists (rest names) (rest li))]))

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


; 1String word -> list-of-words
; produces a list of words with the 1string inserted at the beginning, between all letters, and at the end of the given word
(define (insert-everywhere/word 1str wrd)
   (fix-words wrd (add-letter 1str wrd)))


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


; word word -> word
; returns the first half of a word given it's other half with a new letter appeneded 
(define (find-rest-word orig-word part-word)
  (cond
    [(empty? part-word) (reverse orig-word)]
    [(not (equal? (first orig-word) (first part-word))) (reverse orig-word)]
    [(empty? (rest part-word)) (reverse orig-word)]
    [else (find-rest-word (rest orig-word) (rest part-word))]))
  
