;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |22.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

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

; Xexpr.v2 -> Symbol
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

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

; An Xexpr.v2-Body is one of the following:
; -[List-of Attribute]
; -Xexpr.v2

; [List-of Attribute] Symbol -> [Maybe- String]
; if the attributes list associates the symbol with a string, the function retrieves the string; otherwise it returns #false
(define (find-attr loa symb)
  (if (list? (assq symb loa)) (second (assq symb loa))
      #false))

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of three items:
;   (cons key (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color
; A key is a string recognized by DrRacket as specifying a key
 
; data examples 
(define fsm-traffic-keys
  '(("r" "red" "green") ("g" "green" "yellow") ("y" "yellow" "red")))
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; EX 378 - Modify the rendering function so it overlays the name of the state onto the colored square

; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
       (overlay (text current 24 "black") (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; EX 379 - Test cases for find
(check-expect (find '(("world" 2) ("hello" 3) ("good" 0)) "good") 0)
(check-expect (find fsm-traffic "green") "yellow")

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; EX 381 - definitions of XMachine and X1T written using list
; An XMachine is a nested list of this shape:
;   (list 'machine (list (list initial FSM-State) [List-of X1T]))
; An X1T is a nested list of this shape:
;   (list 'action (list (list state FSM-State) (list next FSM-State)))

; definitions of XMachine and X1T written using cons
; An XMachine is a nested list of this shape:
;   (cons 'machine (cons (cons initial (cons FSM-State '()) [List-of X1T])))
; An X1T is a nested list of this shape:
;   (cons 'action (cons (cons state (cons FSM-State '())) (cons next (cons FSM-State '()))))

; EX 382 - XML config for the BW machine which switches from white to black and back for every key event
(define bw0
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))
 
; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
 
(check-expect (xm-state0 xm0) "red")
 
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
 
(check-expect (xm->transitions xm0) fsm-traffic)
 
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))