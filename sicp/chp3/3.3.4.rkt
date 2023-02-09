#lang sicp

; Section 3.3.4 A Simulator for Digital Circuits

; Representing Wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; Queue pkg
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))


; The Agenda
; made up of time segments
; each time segment is a pair consisting of a number (the time) and a queue that holds the procedures that are scheduled to be run during that time segment
; the agenda itself is a 1D table of time segments
; segments will be sorted in order of increasing time
; current time (i.e., the time of the last action that was processed) is stored at the head of the agenda
; a newly constructed agenda has no time segments and has a current time of 0
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

; To add an action to an agenda, we first check if the agenda is empty. If so,
; we create a time segment for the action and install this in the agenda.
; Otherwise, we scan the agenda, examining the time of each segment.
; If we find a segment for our appointed time, we add the action to the
; associated queue. If we reach a time later than the one to which we are
; appointed, we insert a new time segment into the agenda just before it.
; If we reach the end of the agenda, we must create a new time segment
; at the end.
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

; if deleting the item at the front of the queue in the first time segment makes the time segment empty, we remove the time segment from the list of segments.
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; whenever we extract the first agenda item, which is found at the head of the queue in the first time segment, we also update the current time.
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; Primitive Function Boxes
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; EX 3.28
; define an or-gate as a primitive function box.
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; EX 3.29
; define an or-gate procedure built from and-gates and inverters
(define (or-gate3.29 a1 a2 output)
  (let ((b (make-wire)) (c (make-wire)) (d (make-wire)))
    (inverter a1 b)
    (inverter a2 c)
    (and-gate b c d)
    (inverter d output)
    'ok))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; EX 3.30
; write the ripple-carry-adder procedure that generates a ripple-carry adder
(define (ripple-carry-adder lA lB lS c)
  (cond
    ((null? lA)
     (list c))
    (else
     (let ((ck (make-wire)))
       (full-adder (car lA) (car lB) c (car lS) ck)
       (cons (car lS) (ripple-carry-adder (cdr lA) (cdr lB) (cdr lS) ck))))))

; The delay needed to obtain the complete output from an n-bit ripple-carry adder is 4n and-gates, 3n or-gates, and 2n inverters

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; EX 3.31
; if accept-action-procedure! did not run the given procedure whenever a new action procedure is added to a wire the delay and signal on the wire would never
; change because the action procedure would just be defined within the wire object but never executed. the procedure must be ran for the action to be added to the agenda for execution
; and subsequent effect on the the wire's state.

; EX 3.32
; explain why first in, first out processing must be used on the procedures within each time segment. in particular, trace the behavior of an and-gate whose inputs change from 0, 1 to
; 1, 0 in the same segment and say how the behavior would differ if we stored a segment's procedures in an ordinary list, adding and removing procedures only at the front (last in,
; first out).

; answer from CrazyAlvaro:
; For and-gate: both a1 a2 have and-action-procedure which contains after-delay

; the point here is that new-value is calculated before delay
; if the segments' list has LIFO, and the first wire a1 change first, then the result will be incorrect
; (a1, a2)
; (0, 1) initial
; (1, 1) a1 0 -> 1, then a1's and-action-procedure called, a1's new-value = 1, lambda procedure inserted into segment
; (1, 0) a2 1 -> 0, then a2's and-action-procedure called, a2's new-value = 0, lambda procedure inserted into segment
; propogate, after-delay
; a2's (lambda() (set-signal! output new-value)) called, set output = 0 
; a1's (lambda() (set-signal! output new-value)) called, set output = 1

; so after this propogation we got output = 1, which is not correct