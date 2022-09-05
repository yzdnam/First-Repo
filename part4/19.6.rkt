;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EX 328
; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(((world) bye) bye) 'world '42)
              '(((42) bye) bye))
(check-expect (substitute '(fart (come shine boot ((world) bye) bye)) 'boot '42)
              '(fart (come shine 42 ((world) bye) bye)))

(define (atom? item)
  (or (number? item)
      (string? item)
      (symbol? item)))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))