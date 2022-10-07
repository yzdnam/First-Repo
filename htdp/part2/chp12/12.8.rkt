;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.8|) (read-case-sensitive #t) (teachpacks ((lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
;(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; EX 226
; string -> boolean
; equality predicate for states
(define (state=? str)
  (cond
    [(or (equal? str "white")
         (equal? str "yellow")
         (equal? str "orange")
         (equal? str "green")
         (equal? str "red")
         (equal? str "blue")
         (equal? str "black")) #true]
    [else #false]))

(check-expect (state=? "black") #true)
(check-expect (state=? "fart") #false)

; Example FSM
;(define fsm-traffic
;  (list (make-transition "red" "green")
;        (make-transition "green" "yellow")
;        (make-transition "yellow" "red")))

; EX 227
; Example FSM
;(define bw-machine
;  (list (make-transition "black" "white")
;        (make-transition "white" "black")))

; the design recipe for world programs tells us to differentiate
; between the dynamic and static aspects of the world. since only
; the current state of the machine changes in a given FSM, it is
; to be assumed that a SimulationState.v1 is an FSM-State

; FSM -> ???
; match the keys pressed with the given FSM 
;(define (simulate.v1 fsm0)
;  (big-bang initial-state
;    [to-draw render-state.v1]
;    [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
   cs)

; find-next-state.v1 cannot find the next state without being
; given the FSM so the state of the world must include both the
; current state of the FSM and the FSM itself

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

(define-struct fsm [initial transitions final])
; an FSM.v2 is:
;  (make-fsm FSM-State LOT FSM-State)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

(define ERROR-STATE "red")

; FSM.v2 -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (fsm-simulate an-fsm)
  (big-bang (make-fsm (fsm-initial an-fsm) (fsm-transitions an-fsm) (fsm-final an-fsm))
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when done? error-or-final]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
;(check-expect (state-as-colored-square
;                (make-fs fsm-traffic "red"))
;              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from an-fsm and ke
(define (find-next-state an-fsm ke)
  (cond
    [(key-compatible? (fsm-transitions an-fsm) (fsm-initial an-fsm) ke)
     (make-fsm
      (find-next (fsm-transitions an-fsm) (fsm-initial an-fsm) ke)
      (fsm-transitions an-fsm)
      (fsm-final an-fsm))]
    [else (error-fsm an-fsm)]))

; initial LOTS string -> boolean
; returns true if the key-string is associated with an fsm's current state in an fsm's list of transitions
(define (key-compatible? lots current ke)
  (cond
    [(empty? lots) #false]
    [else (cond
            [(and
              (equal? (ktransition-current (first lots)) current)
              (equal? (ktransition-key (first lots)) ke))
             #true]
            [else (key-compatible? (rest lots) current ke)])]))

; EX 228
; LOTS FSM-State string -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(define (find-next transitions current ke)
  (cond
    [(and
     (equal? (ktransition-current (first transitions)) current)
     (equal? (ktransition-key (first transitions)) ke))
       (ktransition-next (first transitions))]
    [else (find-next (rest transitions) current ke)]))

; FSM -> FSM
; returns an fsm that signals that a wrong keystroke has been pressed
(define (error-fsm an-fsm)
  (make-fsm ERROR-STATE (fsm-transitions an-fsm) (fsm-final an-fsm)))

; FSM -> boolean
; returns true if the initial state equals the final state
; or if the initial state is the defined error state
(define (done? an-fsm)
  (cond
    [(or
      (equal? (fsm-initial an-fsm) ERROR-STATE)
      (equal? (fsm-initial an-fsm) (fsm-final an-fsm))) #true]
    [else #false]))

; FSM -> image
; returns the image for the final or error state
(define (error-or-final an-fsm)
  (cond
    [(equal? (fsm-initial an-fsm) ERROR-STATE)
     (square 100 "solid" ERROR-STATE)]
    [(equal? (fsm-initial an-fsm) (fsm-final an-fsm))
     (square 100 "solid" (fsm-final an-fsm))]))

;(check-expect
;  (find-next-state (make-fs fsm-traffic "red") "n")
;  (make-fs fsm-traffic "green"))
;(check-expect
;  (find-next-state (make-fs fsm-traffic "red") "a")
;  (make-fs fsm-traffic "green"))



(define fsm109 (make-fsm "black" (list
                (make-ktransition "black" "a" "yellow")
                (make-ktransition "yellow" "b" "yellow")
                (make-ktransition "yellow" "c" "yellow")
                (make-ktransition "yellow" "d" "green")
                ) "green"))
                