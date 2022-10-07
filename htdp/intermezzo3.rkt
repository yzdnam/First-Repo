;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define width 20)

(for/list ([width 10]
           [height width])
  (list width height))

;(for*/list ([width 10]
;            [height width])
;  (list width height))

; list -> list-of pairs
; produces a list of the same items in the given list paired with their relative
; indexes
(define (enumerate lot)
  (local (
          (define index-list (build-list (length lot) (lambda (i) i))))
    (map list lot index-list)))

(for*/list ([i 2] [j '(a b)])
  (list i j))

; list list -> list-of pairs
; produces pairs of all items from the two given lists
(define (cross l1 l2)
  (local (; X X -> list-of pair
          (define (create-pair x y)
            (list x y) ))
    (foldr append '() (map (lambda (l1i) (map (lambda (l2i) (create-pair l1i l2i)) l2)) l1))))

(check-expect (cross (list 1 2) (list 3 4)) (list (list 1 3) (list 1 4) (list 2 3) (list 2 4)))

; [list-of x] [x -> boolean] -> x or #false
; version of andmap that returns the last item in the given list if andmap returns #true
(define (and-map lox bool-func)
  (cond
    [(equal? (andmap bool-func lox) #true) (first (reverse lox))]
    [else #false]))

; [list-of numbers] [number -> boolean] -> int or #false
; accepts a list consisting of 0 to n-1
; returns the last number in the list if all numbers in the list return #true after being evaluated by the given function 
; #false otherwise
(define (for-and n body-func)
  (and-map (build-list n (lambda (x) x)) body-func))

(check-expect (for-and 10 (lambda (x) (> (- 9 x) 0))) (for/and ([i 10]) (> (- 9 i) 0)))

; adds up the numbers that the loop iterations generate
(define (for-sum str func)
  (foldr + 0 (map func (explode str))))

(check-expect (for-sum "abc" string->int) (for/sum ([c "abc"]) (string->int c)))

; creates a string from the 1string sequence generated
(define (for-string n func)
  (foldr string-append "" (map func (build-list n (lambda (x) x)) )))

(check-expect (for-string 10 (lambda (x) (int->string (+ 97 x)))) (for/string ([j 10]) (int->string (+ 97 j))))

(define (tabulate func n)
  (for/list ([i n]) (func i)))

; EX 307
; string [list-of strings] -> string
; retrieves the first name on the given list that is equal to, or an extension of, the given name
(define (find-name-loop name lon)
  (for/or ([result (for/list ([name-in-list lon])
                     (for/and ([a name-in-list][b name])
                                   (if (equal? a b) name-in-list #f)))]) (if (string? result) result #false)))

(find-name-loop "rob" (list "ark" "fawn" "bob" "robbie" "bill"))

; [list-of string] number -> boolean
; ensures no name on the given list exceeds a given length
(define (names-len<? lon n)
  (for/and ([name lon]) (<= (length (explode name)) n)))

(check-expect (names-len<? (list "rob" "char" "bob") 4) #true)
(check-expect (names-len<? (list "rob" "charlie" "bob") 4) #false)

; list-of X -> X
; returns last item in given list
(define (last-item lot)
  (match lot
    [(cons last '()) last]
    [(cons frst rest) (last-item rest)]))

(check-expect (last-item (list 1 2 3 4 5)) 5)

(define-struct phone [area switch four])

; EX 308
; list-of phones -> list-of phones
; replaces the area code 713 with 281
(define (replace lopn)
  (for/list ((n lopn))
    (match n
      [(phone 713 switch four) (make-phone 281 switch four)]
      [(phone area switch four) (make-phone area switch four)])))


(check-expect (replace (list (make-phone 713 288 2888) (make-phone 555 555 5555)))
              (list (make-phone 281 288 2888) (make-phone 555 555 5555)))
; EX 309
; list-of list-of strings -> list-of number
(define (words-on-line lolos)
  (for/list ((los lolos))
    (match los
      ['() 0]
      [(cons first rest) (+ 1 (length rest))])))

(check-expect (words-on-line (list (list "a" "b" "c") (list) (list "a" "b"))) (list 3 0 2))