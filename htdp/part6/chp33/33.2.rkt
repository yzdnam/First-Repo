;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |33.2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
; PuzzleState -> [List-of PuzzleState]
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination ???
 
;(check-expect (solve initial-puzzle) final-puzzle)
 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (mishcanpuzz-past-states (first (filter final? los)))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))

; EX 520 - why does generating all states reachable with n iterations before looking at states requiring (n + 1) iterations prevent a function from entering an
; infinite loop?
; because the function will include those states that are progressing towards the final state along with the infinitely looping states in its check for completion.
; once a single state reaches the final state, the function will terminate, making the inifinitely looping states irrelevant to the termination of the function

; EX 521 - develop a representation for the states of the missionary-and-cannibal puzzle
(define-struct bank (mish cann))
; mish and cann are both natural numbers
(define-struct mishcanpuzz (start-bank boat end-bank past-states))
; start-bank and end-bank are bank-structures
; a boat is one of the following symbols:
; - 'left
; - 'right
; past-states is a list of states traversed to arrive at the current state represented by start-bank, boat, and end-bank

; Represent an initial, intermediate, and final states using the above data representation
(define initial-mishcanpuzz (make-mishcanpuzz (make-bank 3 3) 'left (make-bank 0 0) '()))
;(define an-intermediate-mishcanpuzz (make-mishcanpuzz (make-bank 1 1) 'left (make-bank 2 2)
;                                                      (list (make-mishcanpuzz (make-bank 0 1) 'right (make-bank 3 2))
;                                                            (make-mishcanpuzz (make-bank 2 1) 'left (make-bank 1 1))
;                                                            (make-mishcanpuzz (make-bank 2 1) 'right (make-bank 2 1))
;                                                            (make-mishcanpuzz (make-bank 2 2) 'left (make-bank 2 0))
;                                                            (make-mishcanpuzz (make-bank 2 2) 'right (make-bank 2 0)))
;(define final-mishcanpuzz (make-mishcanpuzz (make-bank 0 0) 'right (make-bank 3 3)))

; PuzzleState -> Boolean
; detects whether in a given state all people are on the right river bank
(define (final? state)
  (and (zero? (bank-mish (mishcanpuzz-start-bank state)))
       (zero? (bank-cann (mishcanpuzz-start-bank state)))
       (symbol=? 'right (mishcanpuzz-boat state))))

; PuzzleState -> Image
; maps a state of the missionary-and-cannibal puzzle to an image
(define PIECE-RADIUS 10)
(define PIECE-DIAMETER (* PIECE-RADIUS 2))
(define MISH-PIECE (circle PIECE-RADIUS "solid" "black"))
(define CANN-PIECE (circle PIECE-RADIUS "solid" "green"))
(define BANK-WIDTH (* 2 PIECE-DIAMETER))
(define RIVER-WIDTH (* 5 PIECE-DIAMETER))
(define (render-mc state)
  (local ((define number-of-mish (+ (bank-mish (mishcanpuzz-start-bank state)) (bank-mish (mishcanpuzz-end-bank state))))
          (define number-of-cann (+ (bank-cann (mishcanpuzz-start-bank state)) (bank-cann (mishcanpuzz-end-bank state))))
          (define board-piece-height (if (>= number-of-mish number-of-cann) number-of-mish number-of-cann))
          (define board-height (* board-piece-height PIECE-DIAMETER))
          (define bank-board (rectangle BANK-WIDTH board-height "outline" "black"))
          ; Bank -> Image
          (define (place-pieces a-bank)
            (cond
              [(and (zero? (bank-mish a-bank))
                    (zero? (bank-cann a-bank))) bank-board]
              [(and (not (zero? (bank-mish a-bank)))
                    (zero? (bank-cann a-bank))) (overlay/xy (place-pieces (make-bank (sub1 (bank-mish a-bank)) (bank-cann a-bank)))
                                                            0 (* PIECE-DIAMETER (sub1 (bank-mish a-bank))) MISH-PIECE)]
              [(and (zero? (bank-mish a-bank))
                    (not (zero? (bank-cann a-bank)))) (overlay/xy (place-pieces (make-bank (bank-mish a-bank) (sub1 (bank-cann a-bank))))
                                                            PIECE-DIAMETER (* PIECE-DIAMETER (sub1 (bank-cann a-bank))) CANN-PIECE)]
              [(and (not (zero? (bank-mish a-bank)))
                    (not (zero? (bank-cann a-bank)))) (overlay/xy (place-pieces (make-bank (sub1 (bank-mish a-bank)) (sub1 (bank-cann a-bank))))
                                                            0 (* PIECE-DIAMETER (sub1 (if (>= (bank-cann a-bank) (bank-mish a-bank))
                                                                                          (bank-cann a-bank) (bank-mish a-bank))))
                                                                                      (beside MISH-PIECE CANN-PIECE))]))
          (define start-bank-board (place-pieces (mishcanpuzz-start-bank state)))
          (define end-bank-board (place-pieces (mishcanpuzz-end-bank state)))
          (define empty-river-board (rectangle RIVER-WIDTH board-height "solid" "blue"))
          (define boat-image (above (rhombus (* 1.5 PIECE-RADIUS) 120 "solid" "black") (rectangle PIECE-DIAMETER  PIECE-RADIUS "solid" "black")))
          ; Symbol -> Image
          (define (place-boat a-boat)
            (cond
              [(symbol=? a-boat 'left) (overlay/align "left" "middle" boat-image empty-river-board)]
              [(symbol=? a-boat 'right) (overlay/align "right" "middle" boat-image empty-river-board)]))
          (define river-board (place-boat (mishcanpuzz-boat state))))
    (beside start-bank-board river-board end-bank-board)))

;(render-mc final-mishcanpuzz)
;(render-mc an-intermediate-mishcanpuzz)
;(render-mc (make-mishcanpuzz (make-bank 3 1) 'left (make-bank 5 2) '()))
;(render-mc (make-mishcanpuzz (make-bank 1 3) 'left (make-bank 2 5) '()))

; EX 522 - modify the data representation of a mishcannpuzz so that a state records the sequence of states traversed to get there. include an accumulator statement
; and modify final? and render-mc to accomodate the modification
; no modifications to final? or render-mc needed

; EX 523 - design the create-next-states function, it consumes lists of missionary-and-cannibal states and generates the list of all those states that a boat ride
; can reach

; [List-of mishcannpuzz] -> [List-of mishcannpuzz]
(define (create-next-states los)
  (local (; mishcannpuzz -> [List-of mishcannpuzz]
          (define (create-next-states-single state)
            (local ((define current-boat (mishcanpuzz-boat state))
                    (define curr-leftbnk-mish (bank-mish (mishcanpuzz-start-bank state)))
                    (define curr-leftbnk-cann (bank-cann (mishcanpuzz-start-bank state)))
                    (define curr-rightbnk-mish (bank-mish (mishcanpuzz-end-bank state)))
                    (define curr-rightbnk-cann (bank-cann (mishcanpuzz-end-bank state)))
                    (define new-past-states (cons state (mishcanpuzz-past-states state)))
                    (define (chg-state mish-delta cann-delta)
                      (cond
                        [(symbol=? 'left current-boat) (make-mishcanpuzz (make-bank (- curr-leftbnk-mish mish-delta) (- curr-leftbnk-cann cann-delta)) 'right
                                                                      (make-bank (+ curr-rightbnk-mish mish-delta) (+ curr-rightbnk-cann cann-delta)) new-past-states)]
                        [(symbol=? 'right current-boat) (make-mishcanpuzz (make-bank (+ curr-leftbnk-mish mish-delta) (+ curr-leftbnk-cann cann-delta)) 'left
                                                                      (make-bank (- curr-rightbnk-mish mish-delta) (- curr-rightbnk-cann cann-delta)) new-past-states)]))
                    (define (illegal-state? state)
                      (or
                       (and (not (zero? (bank-mish (mishcanpuzz-start-bank state))))
                            (> (bank-cann (mishcanpuzz-start-bank state)) (bank-mish (mishcanpuzz-start-bank state))))
                       (and (not (zero? (bank-mish (mishcanpuzz-end-bank state))))
                            (> (bank-cann (mishcanpuzz-end-bank state)) (bank-mish (mishcanpuzz-end-bank state))))))
                    (define (looping-state? state)
                      (local ((define (same-state? other-state)
                                (and
                                 (equal? (bank-mish (mishcanpuzz-start-bank state)) (bank-mish (mishcanpuzz-start-bank other-state)))
                                 (equal? (bank-cann (mishcanpuzz-start-bank state)) (bank-cann (mishcanpuzz-start-bank other-state)))
                                 (symbol=? (mishcanpuzz-boat state) (mishcanpuzz-boat other-state)))))
                        (ormap same-state? (mishcanpuzz-past-states state)))))
              
              (filter (lambda (x) (not (or (illegal-state? x) (looping-state? x))))
                      (list
                       (chg-state 1 0) (chg-state 2 0) (chg-state 1 1) (chg-state 0 2) (chg-state 0 1))))))
    (cond
      [(empty? los) '()]
      [else
        (foldr (lambda (x y) (append (create-next-states-single x) y)) '() los)])))

;(solve initial-mishcanpuzz)

; EX 524 - revise the solve function to produce the list of states that lead from the initial puzzlestate to the final one

(run-movie 2 (map render-mc (reverse (solve initial-mishcanpuzz))))