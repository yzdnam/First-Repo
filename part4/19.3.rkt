;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EX 316
(define (atom? item)
  (or (number? item)
      (string? item)
      (symbol? item)))

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; EX 317
; count function reorganized with a local expression

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
 (cond
   [(atom? sexp) (local (; Atom -> N 
                         ; counts all occurrences of sy in at 
                         (define (count-atom at)
                           (cond
                             [(number? at) 0]
                             [(string? at) 0]
                             [(symbol? at) (if (symbol=? at sy) 1 0)])))
                    (count-atom sexp))]
   [else (local (; SL -> N 
                 ; counts all occurrences of sy in sl 
                 (define (count-sl sl)
                   (cond
                     [(empty? sl) 0]
                     [else
                       (+ (count (first sl) sy) (count-sl (rest sl)))])))
           (count-sl sexp))]))

; EX 318
; S-expr -> Number
; determines the depth of an S-expression
; an atom has a depth of 1. The depth of a list of S-expressions is the max depth of its items plus 1
(define (depth sexpr)
  (cond
    [(atom? sexpr) 1]
    [else (local (; SL -> N
                  ; determines the depth of an SL
                  ; max depth of its items plus 1
                  (define (depth-sl sl)
                    (cond
                      [(empty? sl) 1]
                      [else
                       (cond
                         [(atom? sl) (+ 1 (depth sl))]
                         [else (+ 1 (depth-sl (first sl)))]
                         )])))
            (cond
              [(>= (depth-sl (first sexpr))(depth-sl (rest sexpr))) (depth-sl (first sexpr))]
              [else (depth (rest sexpr))]))]))

(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)
(check-expect (depth (list (list (list 'world) 'hello) 'hello)) 4)
(check-expect (depth (list 'world (list (list (list (list 'hello)) 'world)))) 6)

; EX 319
; S-expr Symbol Symbol -> S-expr
; replaces all of the first given symbol with the second given symbol in the given S-expr
(define (substitute sexpr old new)
  (local (
          (define (sub-atom atom)
            (cond
              [(equal? atom old) new]
              [else atom]))

          (define (sub-sl sl)
            (cond
              [(empty? sl) '()]
              [else (cond
                      [(atom? (first sl)) (cons (sub-atom (first sl)) (sub-sl (rest sl)))]
                      [else (cons (sub-sl (first sl)) (sub-sl (rest sl)))])])))
          
  (cond
    [(atom? sexpr) (sub-atom sexpr)]
    [else (sub-sl sexpr)])))


(check-expect (substitute 'world 'world 'hello) 'hello)
(check-expect (substitute '(world hello) 'world 'hello) '(hello hello))
(check-expect (substitute '(((world) hello) hello) 'hello 'fart) '(((world) fart) fart))

; EX 320
; GIVEN DATA DEFINITION
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
	
; An Atom is one of: 
; – Number
; – String
; – Symbol 

; ALTERNATIVE DATA DEFINTION
; expand the first clause of the first data definition into the 3 clauses of Atom
; and use the List-of Abstraction in the second data definition

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – SL
 
; An SL is one of: 
; – '()
; – [List-of S-expr]

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(define (count-alt sexpr sy)
  (cond
    [(number? sexpr) 0]
    [(string? sexpr) 0]
    [(symbol? sexpr) (if (symbol=? sexpr sy) 1 0)]
    [(list? sexpr) (local (
                           (define (count-sl-alt sexpr)
                             (cond
                               [(empty? sexpr) 0]
                               [else
                                 (+ (count-alt (first sexpr) sy) (count-sl-alt (rest sexpr)))])))
                     (count-sl-alt sexpr))]))

(check-expect (count-alt 'world 'hello) 0)
(check-expect (count-alt '(world hello) 'hello) 1)
(check-expect (count-alt '(((world) hello) hello) 'hello) 2)

; 2ND ALT DATA DEFINITION

; An S-expr is one of:
; - Number
; - String
; - Symbol
; - '()
; - [List-of S-expr]

(define (count-alt2 sexpr sy)
  (cond
    [(number? sexpr) 0]
    [(string? sexpr) 0]
    [(symbol? sexpr) (if (symbol=? sexpr sy) 1 0)]
    [(empty? sexpr) 0]
    [(list? sexpr) (+ (count-alt2 (first sexpr) sy) (count-alt2 (rest sexpr) sy))]))

(check-expect (count-alt2 'world 'hello) 0)
(check-expect (count-alt2 '(world hello) 'hello) 1)
(check-expect (count-alt2 '(((world) hello) hello) 'hello) 2)

; EX 321
; GIVEN DATA DEFINITION
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
	
; An Atom is one of: 
; – Number
; – String
; – Symbol

; Abstracted data definitions for S-expr and SL
; They're already abstracted
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
	
; An Atom is one of: 
; – X
; – Y
; – Z
; - ...
