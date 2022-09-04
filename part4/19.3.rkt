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
                       (+ (depth (first sl)) (depth-sl (rest sl)))])))
            (depth-sl sexpr))]))

(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)
