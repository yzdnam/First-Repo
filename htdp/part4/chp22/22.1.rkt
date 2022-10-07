;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |22.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])
; adds content

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; EX 363
; refinement of Xexpr.v2 definition
; An Xexpr.v2 is a list whose first item is a Symbol followed by one of the following:
; - Xexpr.2
; - Attributes
; Attributes are one of the following:
; -'()
; - (cons Symbol (cons String Attributes))

; EX 364
; 1. <transition from="seen-e" to="seen-f" />
; '(transition ((from "seen-e") (to "seen-f")))
; 2. <ul><li><word /><word /></li><li><word /></li></ul>
; '(ul (li (word) (word)) (li (word) ))

; EX 365
; 1. '(server ((name "example.org")))
; <server name="example.org" />
; 2. '(carcas (board (grass)) (player ((name "sam"))))
; <carcas><board><grass /></board><player name="sam" /></carcas>
; 3. '(start)
; <start />

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; [List-of Attribute] or Xexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; EX 366
; Xexpr.v2 -> Symbol
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

; Xexpr.v2 -> Symbol
; retrieves the content of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) (list (list 'action)))
(check-expect (xexpr-content e3) (list (list 'action)))
(check-expect (xexpr-content e4) (list (list 'action) (list 'action)))

; EX 367
; xexpr-attr does not contain a self-reference because the function retrieves the attributes of the given Xexpr, not any of the nested Xexprs

; EX 368
; data definition to replace "[List-of Attribute] or Xexpr.v2" in the signature for list-of-attributes?
; An Xexpr.v2-Body is one of the following:
; -[List-of Attribute]
; -Xexpr.v2

; EX 369
; [List-of Attribute] Symbol -> [Maybe- String]
; if the attributes list associates the symbol with a string, the function retrieves the string; otherwise it returns #false
(define (find-attr loa symb)
  (if (list? (assq symb loa)) (second (assq symb loa))
      #false))

(define example-loa '((sum "one")(cum "two")(dum "three")))
(check-expect (find-attr example-loa 'sum) "one")
(check-expect (find-attr example-loa 'dum) "three")
(check-expect (find-attr example-loa 'lum) #false)