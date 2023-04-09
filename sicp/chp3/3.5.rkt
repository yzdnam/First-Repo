#lang sicp

; Section 3.5 Streams

; 3.5.1 Streams are Delayed Lists

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (test-delay b) (memo-proc (lambda () b)))
(define (test-force b) (b))

;(define (cons-stream a b)
;  (cons a (delay b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; EX 3.50
; complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments, analogous to the map procedure depicted on page 143 of the text
(define (stream-map3.50 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map3.50
              (cons proc (map stream-cdr argstreams))))))

;(define test-stream (stream-map3.50 + (stream-enumerate-interval 1 5) (stream-enumerate-interval 11 15) (stream-enumerate-interval 21 25)))
;test-stream
;(stream-car (stream-cdr test-stream))
;(stream-car (stream-cdr (stream-cdr test-stream)))
;(stream-car (stream-cdr (stream-cdr (stream-cdr test-stream))))
;(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr test-stream)))))
;(stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr test-stream)))))

; EX 3.51
(define (show x)
  (display-line x)
  x)

; What does the interpreter print in response to evaluating each expression in the following sequence?
;(define x
;  (stream-map show
;              (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

; EX 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
;(stream-ref y 7)
;(display-stream z)
; What is the value of sum after each of the the above expressions is evaluated?
; sum becomes 210 after seq is evaluated and remains 210 after the remaining expressions are evaluated.
; Would the responses to the stream-ref and display-stream expressions have differed if we had implemented (delay <exp>) simply as (lambda () <exp>) without using memo-proc?
; the responses would not have differed because there is no expression evaluated during the evaluation of seq that is evaluated more than once.

; 3.5.2 Infinite Streams

(define (add-streams s1 s2) (stream-map3.50 + s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (div-streams s1 s2) (stream-map3.50 / s1 s2))

; EX 3.53
; without running the program, describe the elements of the stream defined by
; (define s (cons-stream 1 (add-streams s s)))

; elements of s evaluated to the nth member are defined as such:
; {2^0, 2^1, 2^2, ... , 2^n}
; in other words, the stream-car of each successive stream-cdr of s is twice that of the stream-car of the stream-cdr that preceded it.

; EX 3.54
; define mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams.
(define (mul-streams s1 s2) (stream-map3.50 * s1 s2))

; Use this with the stream of integers to complete the definition of the stream whose nth element (counting from 0) is n+1 factorial:
(define factorials
  (cons-stream 1 (mul-streams (add-streams ones integers) factorials)))

; EX 3.55
; define partial-sums which takes as argument a stream S and returns the stream whose elements are s0, s0+s1, s0+s1+s2,...
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

; EX 3.56
; use the procedure below, merge, to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define s3.56 (cons-stream 1 (merge (merge (scale-stream s3.56 2) (scale-stream s3.56 3)) (scale-stream s3.56 5))))

(define (show-stream-to n s)
  (define (show-stream-to/a a)
    (if (eq? a n)
        (stream-ref s n)
        (begin
          (display (stream-ref s a))
          (newline)
          (show-stream-to/a (inc a)))))
  (show-stream-to/a 0))

;(show-stream-to 20 s3.56)

; EX 3.57

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; how many additions are performed when we compute the nth Fibonacci number using the definition of fibs based on the add-streams procedure?
; n-1 additions

; show that the number of additions would be exponentially greater if we had implemented (delay <exp>) simply as (lambda () <exp>), without using the optimization provided by
; memo-proc
; see notes

; EX 3.58
; give an interpretation of the stream computed by the following procedure:
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
; it returns the floating point representation of (/ num dem) with radix as the base

; EX 3.59
; a. define integrate-series that takes as input a stream a0, a1, a2,... representing a power series and returns the stream a0, 1/2a1, 1/3a2,...of coefficients of the non-constant
; terms of the integral of the series
(define (integrate-series s)
  (let ((fractions (div-streams ones integers)))
    (mul-streams s fractions)))

; b. show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define neg-ones (cons-stream -1 neg-ones))

(define cosine-series (cons-stream 1 (integrate-series (mul-streams sine-series neg-ones))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))

; EX 3.60
; define a procedure for multiplying series:

(define (mul-series s1 s2)
  (let ((first-s1 (stream-car s1))
        (first-s2 (stream-car s2)))
    (cons-stream (* first-s1 first-s2) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2)))))

; test the above procedure by verifying that sin^2(x) + cos^2(x) = 1
(define sin2+cos2 (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

; EX 3.61
; write invert-unit-series that computes 1/S for a power series S with constant term 1
(define (invert-unit-series s)
  (define series
    (cons-stream 1 (scale-stream (mul-series (stream-cdr s) series) -1)))
  series)

(define inv-cos (invert-unit-series cosine-series))
;(stream-cdr (stream-cdr inv-cos))
;(stream-cdr (stream-cdr (stream-cdr (stream-cdr inv-cos))))

; EX 3.62
; define div-series that divides two power series. div-series should work for any two series, provided that the denominator series begins with a nonzero constant term. if the
; denominator has a zero constant term, then div-series should signal an error
(define (div-series num denom)
  (if (zero? (stream-car denom))
      (error "denominator series must begin with a nonzero constant term.")
      (mul-series num (invert-unit-series denom))))

(define tan-series (div-series sine-series cosine-series))
;(stream-cdr tan-series)
;(stream-cdr (stream-cdr (stream-cdr tan-series)))
;(stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr tan-series)))))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

; EX 3.63
; explain why the following sqrt-stream is less efficient than the one below it:
(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

(define (good-sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

; the not good sqrt-stream will create a new stream every time its stream-cdr is evaluated. this new stream will not have access to its parent's previous iterations and will need
; to recompute all of the previous sqrt-improve iterations. with the good-sqrt-stream, since guesses is a constant, each successive call to guesses is able to access its memo of
; previous iterations saving the procedure from an exponential increase in steps with each iteration.

; EX 3.64
; write stream-limit that takes as arguments a stream and a number (the tolerance). it should examine the stream until it finds two successive elements that differ in absolute value
; by less than the tolerance and return the 2nd of the two elements
(define (stream-limit s tol)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s2 s1)) tol)
        s2
        (stream-limit (stream-cdr s) tol))))

; EX 3.65
; use the series given in the text to compute 3 sequences of approximations to the natural logarithm of 2 in the same way done for pi in the text.
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (inc n)))))

(stream-ref (ln2-summands 1) 0)
(stream-ref (ln2-summands 1) 1)
(stream-ref (ln2-summands 1) 2)
(stream-ref (ln2-summands 1) 3)

(define ln2-stream1
  (partial-sums (ln2-summands 1))) ;;; returns between 0.692945 and 0.693348

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define ln2-stream2
  (euler-transform ln2-stream1)) ;;; returns 0.6931471805

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define ln2-stream3
  (accelerated-sequence euler-transform ln2-stream1)) ;; returns 0.6931471805599454 after 10 iterations and then outputs become undefined.

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; EX 3.66
; examine and comment on the stream (pairs integers integers). particularly, approximately how many pairs precede the pair (1, 100)? (99, 100)? (100, 100)?
(define counted-pairs ;;; shows # of iteration along with pair
  (cons-stream (cons (stream-car integers) (stream-car (pairs integers integers)))
               (stream-map3.50 (lambda (count pair) (cons count pair)) (stream-cdr integers) (stream-cdr (pairs integers integers)))))
; for every pair (i, j), for each i, j will increment about every 2^i iterations. a new i, i(sub)n, will generate about every (2^(i(sub)n)) iterations

; EX 3.67
; modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers (without the condition i =< j)
(define (pairs3.67 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (pairs3.67 (stream-cdr s) (stream-cdr t))))))

; EX 3.68
; what happens if we define pairs as follows:
(define (pairs3.68 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs3.68 (stream-cdr s) (stream-cdr t))))
; the procedure enters an infinite loop when the second call to interleave calls stream-car on (pairs (stream-cdr s) (stream-cdr t)). this version of the pair procedure
; provides a stream-car implicitly through interleave so when interleave attempts to find the stream-car by evaluating (pairs (stream-cdr s) (stream-cdr t)), it ends up calling itself
; and entering the loop.

; EX 3.69
; write triples that takes three infinite streams, S, T, and U, and produces the stream of triples (Si, Tj, Uk) such that i <= j <= k. use triples to generate the stream of all
; Pythagorean triples of positive integers, i.e., the triples (i, j, k) such that i <= j and i^2 + j^2 = k^2
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map3.50 (lambda (x) (cons (stream-car s) x))
                    (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythag-triple? trip)
  (= (+ (square (car trip)) (square (cadr trip))) (square (caddr trip))))

(define pythag-triples (stream-filter pythag-triple? (triples integers integers integers)))

; TODO understand xdavidliu's solution
(define first-of-integer-pair 
   (stream-map car (pairs integers integers))) 
  
 (define (triples-alt s t u) 
   (let ((pairs-tu (pairs t u))) ;; compute pairs only *once* 
     (define (rec si i ptu top-i) 
       (cons-stream 
        (cons (stream-car si) (stream-car ptu)) 
        (if (= i (stream-car top-i)) 
            (rec s 1 (stream-cdr ptu) (stream-cdr top-i)) 
            ;; restart s cycle with next ptu 
            (rec (stream-cdr si) (inc i) ptu top-i)))) 
     (rec s 1 pairs-tu first-of-integer-pair)))

(define pythag-triples-alt (stream-filter pythag-triple? (triples-alt integers integers integers)))
; although the alternate rendition of triples by xdavidliu appears to compute triples with less calls to pairs, it took just as long to reach the (15 20 25) pythagorean triple as it
; did for the original triples procedure

; EX 3.70
; write a procedure, merge-weighted, which is a procedure that computes the weight of a pair, and is used to determine the order in which elements should appear in the resulting
; merged stream
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1car-weight (weight s1car))
                 (s2car-weight (weight s2car)))
             (cond ((< s1car-weight s2car-weight)
                    (cons-stream
                     s1car
                     (merge-weighted (stream-cdr s1) s2 weight)))
                   ((> s1car-weight s2car-weight)
                    (cons-stream
                     s2car
                     (merge-weighted s1 (stream-cdr s2) weight)))
                   (else
                    (cons-stream
                     s1car
                     (cons-stream
                      s2car
                      (merge-weighted (stream-cdr s1)
                                      (stream-cdr s2) weight))))))))))

; use merge-weighted to generalize pairs to a procedure, weighted-pairs, that takes two streams and a weighting function, and generates the stream of pairs, ordered according to
; weight
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

; a. the stream of all pairs of positive integers (i, j) with i<=j ordered according to the sum i + j
(define (sum-weight pair)
  (+ (car pair) (cadr pair)))

(define sum-ordered-pairs (weighted-pairs integers integers sum-weight))

; b. stream of all pairs of positive integers (i, j) with i<=j, where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2i+3j+5ij
(define (div-by-235? i)
  (or (zero? (remainder i 2))
      (zero? (remainder i 3))
      (zero? (remainder i 5))))

(define (not-235-div? i)
  (not (div-by-235? i)))

(define ints-not-235-div (stream-filter not-235-div? integers))

(define (weird-sum-weight pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define weird-ordered-pairs-not-235-div (weighted-pairs ints-not-235-div ints-not-235-div weird-sum-weight))

; EX 3.71
; write a procedure to generate the ramanujan numbers
(define (cube x)
  (* x x x))
(define (cubes-sum-weight pair)
  (+ (cube (car pair)) (cube (cadr pair))))
(define pairs-ordered-as-sums-of-two-cubes (weighted-pairs integers integers cubes-sum-weight))
; checks the equality of each item with the next one in a stream
(define (rj-number? s)
  (if (eq? (cubes-sum-weight (stream-car s)) (cubes-sum-weight (stream-car (stream-cdr s))))
      (cons-stream (cubes-sum-weight (stream-car s)) (rj-number? (stream-cdr s)))
      (rj-number? (stream-cdr s))))

(define rj-numbers (rj-number? pairs-ordered-as-sums-of-two-cubes))

; EX 3.72
; generate a stream of all numbers that can be written as the sum of two squares in 3 different ways (showing how they can be so written)
(define (square-sum-weight pair)
  (+ (square (car pair)) (square (cadr pair))))

(define pairs-ordered-as-sums-of-two-squares (weighted-pairs integers integers square-sum-weight))

(define (ex3.72 s)
  (define (check-seq n)
    (let ((s1 (stream-ref s n))
          (s2 (stream-ref s (inc n)))
          (s3 (stream-ref s (+ n 2))))
      (if (and (eq? (square-sum-weight s1) (square-sum-weight s2))
               (eq? (square-sum-weight s1) (square-sum-weight s3)))
          (cons-stream (list s1 s2 s3) (check-seq (inc n)))
          (check-seq (inc n)))))
  (check-seq 0))

(define 3.72solution (ex3.72 pairs-ordered-as-sums-of-two-squares))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; EX 3.73
; write a procedure, RC, that models an RC circuit consisting of a resistor of resistance R and a capacitor of capacitance C in series. it should return a procedure that takes as
; inputs a stream representing the current, i, and an initial value for the capacitor voltage v-sub-0 and produces as output the stream of voltages v
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R) (integral (scale-stream i (/ 1 C)) v0 dt))))

(define (sign-change-detector x y)
  (cond
    ((>= x 0) (if (>= y 0) 0 1))
    (else (if (>= y 0) -1 0))))

; placeholder stream to prevent ex3.74 from throwing an error
(define sense-data integers)

; EX 3.74
; complete the zero-crossings procedure below which is approximately equivalent to the longer version of the procedure above it
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))
(define zero-crossings-long
  (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map3.50 sign-change-detector
                  sense-data
                  (cons-stream 0 sense-data)))

; EX 3.75
; correct the following procedure which attempts to extract the zero crossings from the signal constructed by averaging each value of the sense data with the previous value
; corrected>>>;;; NOT, see if you can recall the correct solution from the website in the morning... ;; DONE
(define (make-zero-crossings3.75 input-stream last-val last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-val)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings3.75
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

; EX 3.76
; write a procedure, smooth, that takes a stream as input and produces a stream in which each element is the average of two successive input stream elements.
(define (smooth s)
  (let ((avpt (/ (+ (stream-car (stream-cdr s))
                    (stream-car s))
                 2)))
    (cons-stream avpt (smooth (stream-cdr s)))))
; then use smooth as a componenet to implement the zero-crossing detector in a more modular style
(define (make-zero-crossings3.76 input-stream)
  (let ((smooth-stream (smooth input-stream)))
    (define zero-crossings
      (stream-map3.50 sign-change-detector
                      smooth-stream
                      (cons-stream 0 smooth-stream)))
    zero-crossings))

; EX 3.77
; modify the procedure below so that it expects the integrand as a delayed argument and hence can be used in systems with loops
(define (integral3.77 delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? (force delayed-integrand))
       the-empty-stream
       (let ((integrand (force delayed-integrand)))
         (integral3.77 (delay (stream-cdr integrand))
                       (+ (* dt (stream-car integrand))
                          initial-value)
                       dt)))))

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral3.77 (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)

; EX 3.78
; write solve-2nd that takes as arguments the constants a, b, and dt and the initial values y0 and dy0 for y and dy/dt and generates the stream of successive values of y
(define (solve-2nd a b dt y0 dy0)
  (let ((y '*unassigned*)
        (dy '*unassigned*)
        (ddy '*unassigned*))
    (set! y (integral3.77 (delay dy) y0 dt))
    (set! dy (integral3.77 (delay ddy) dy0 dt))
    (set! ddy (add-streams (scale-stream dy a) (scale-stream y b)))
    y))

; EX 3.79
; generalize the solve-2nd procedure of EX 3.78 so that it can be used to solve general second-order differential equations d2y/dt2 = f(dy/dt, y)
(define (solve-2nd3.79 f y0 dy0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*)
        (ddy '*unassigned*))
    (set! y (integral3.77 (delay dy) y0 dt))
    (set! dy (integral3.77 (delay ddy) dy0 dt))
    (set! ddy (stream-map3.50 f dy y))
    y))

; EX 3.80
; write a procedure RLC that takes as arguments the parameters R, L, and C of the circuit and the time increment dt. RLC should produce a procedure that takes the initial values of
; the state variables, vC0 and iL0, and produces a pair (using cons) of the streams of states vC and iL
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (let ((vC '*unassigned*)
          (iL '*unassigned*)
          (dvC '*unassigned*)
          (diL '*unassigned*))
      (set! vC (integral3.77 (delay dvC) vC0 dt))
      (set! iL (integral3.77 (delay diL) iL0 dt))
      (set! dvC (scale-stream iL (/ -1 C)))
      (set! diL (add-streams (scale-stream vC (/ 1 L)) (scale-stream iL (/ (* -1 R) L))))
    (stream-map3.50 cons vC iL))))

(define rlc3.80 (RLC 1 1 0.2 0.1))
(define rlc3.80stream (rlc3.80 10 0))

;(define random-numbers
;(cons-stream
;random-init
;(stream-map rand-update random-numbers)))

; EX 3.81
; produce a stream formulation of a random number generator that operates on an input stream of requests to generate a new random number or to reset the sequence to a specified value
;(define (random-num-stream request-stream)
;  (define (reset x)
;    (lambda (random-number)
;      x))
;  (define num-stream
;    (cons-stream
;     random-init
;     (stream-map3.50 apply request-stream num-stream)))
;  num-stream)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((boundary-area (* (- x2 x1) (- y2 y1))))
    (define (P-experiment) (P (random-in-range x1 x2) (random-in-range y1 y2)))
    (* (monte-carlo trials P-experiment) boundary-area)))

(define (unit-circle-predicate x y)
  (>= 1 (+ (square (- x 1)) (square (- y 1)))))

 ;(estimate-integral unit-circle-predicate 0.0 5.0 0.0 5.0 1000000)

; EX 3.82
; redo EX 3.5 on Monte Carlo integration in terms of streams
(define (estimate-integral3.82 P x1 x2 y1 y2)
  (define (experiment-stream)
    (cons-stream
     (P (random-in-range x1 x2) (random-in-range y1 y2))
     (experiment-stream)))
  (let ((boundary-area (* (- x2 x1) (- y2 y1))))
    (stream-map3.50 (lambda (p) (* boundary-area p)) (monte-carlo (experiment-stream) 0 0))))

;(display-stream (estimate-integral3.82 unit-circle-predicate 0.0 2.0 0.0 2.0))
; Good. estimates become more accurate as the predicate's area becomes a larger proportion of the boundary area

(define (solve0 f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral3.77 (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b)
      y)))

;(stream-ref (solve0 (lambda (y) y)
;                   1
;                   0.001)
;            1000)