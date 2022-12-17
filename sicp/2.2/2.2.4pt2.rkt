 #lang sicp 
 (#%require sicp-pict) 

 ;; Exercise 2.49 - Use the segments->painter procedure provided by the library 
 ; Solution pulled from internet due to confusion with prior exercises and actual graphics implmentation in sicp-pict
 ;; Procedures to use: vect, segment; instead of make-vect and make-segment 
  
 ;; Exercise 2.49a 
 (define outline 
   (segments->painter 
    (list 
     (segment (vect 0.0 0.0) (vect 0.0 1.0)) 
     (segment (vect 0.0 0.0) (vect 1.0 0.0)) 
     (segment (vect 0.0 1.0) (vect 1.0 1.0)) 
     (segment (vect 1.0 0.0) (vect 1.0 1.0)))))

;; (paint outline) 
  
 ;; Exercise 2.49b 
 (define x-painter 
   (segments->painter 
    (list 
     (segment (vect 0.0 0.0) (vect 1.0 1.0)) 
     (segment (vect 0.0 1.0) (vect 1.0 0.0))))) 
  
 ;; (paint x-painter) 
  
 ;; Exercise 2.49c 
 (define diamond 
   (segments->painter 
    (list 
     (segment (vect 0.0 0.5) (vect 0.5 1.0)) 
     (segment (vect 0.5 1.0) (vect 1.0 0.5)) 
     (segment (vect 1.0 0.5) (vect 0.5 0.0)) 
     (segment (vect 0.5 0.0) (vect 0.0 0.5)))))

; 2.49d from Lera:
  
 (define (do-many-vectors x-coords y-coords) 
   (let ((coeff (/ 1 4.8)))  
     (map (lambda (x y) (vector-scale  coeff (make-vect x y))) x-coords y-coords))) 
     ;; I just measured  with a ruler all distances on the screen and then divided by side of frame all measured distances  
  
 (define (make-many-segments start-vectors end-vectors) 
   (map (lambda (start-vector end-vector) (make-segment start-vector end-vector)) 
        start-vectors 
        end-vectors)) 
  
 (define start-vectors (do-many-vectors 
                        (list 1.2 1.7 1.5 0.7 0 0.7 1.5 1.9 1.7 2.9 3.1 2.9 3.6 2.9 2.9 2.9 2.4) 
                        (list 0 2.35 2.7 1.9 4.0 2.8 3.05 3.05 3.9 4.8 3.9 3.05 3.05 2.1 2.1 0 1.4))) 
 (define end-vectors (do-many-vectors 
                      (list 1.7 1.5 0.7 0 0.7 1.5 1.9 1.7 1.9 3.1 2.9 3.6 4.8 4.8 3.6 2.4 2.0) 
                      (list 2.35 2.7 1.9 3.1 2.8 3.05 3.05 3.9 4.8 3.9 3.05 3.05 1.8 0.75 0 1.4 0))) 
  
 (define list-of-wave (make-many-segments start-vectors end-vectors)) 
  
 (define wave (segments->painter list-of-wave))

; 2.49d from physjam:
(define wave-alt 
   (segments->painter (list 
                       (make-segment (make-vect .25 0) (make-vect .35 .5)) ; commented out segments are those found on borders of frame
                       (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                       (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                       (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                    ;   (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                       (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                       (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                       (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                       (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                       (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                     ;  (make-segment (make-vect .4 1) (make-vect .6 1)) 
                       (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                       (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                       (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                       (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                    ;   (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                       (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                       (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                    ;   (make-segment (make-vect .75 0) (make-vect .6 0)) 
                       (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                       (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                    ;   (make-segment (make-vect .4 0) (make-vect .25 0)) ; the following three segments are the solution to EX 2.52a :
                       (make-segment (make-vect .42 .78) (make-vect .435 .72))
                       (make-segment (make-vect .435 .72) (make-vect .565 .72))
                       (make-segment (make-vect .565 .72) (make-vect .58 .78))
                       ))) 


; EX 2.50
; define flip-horiz and rotate180 and rotate270
(define (flip-horiz-alt painter)
  (transform-painter
   painter
   (vect 1.0 0.0)
   (vect 0.0 0.0)
   (vect 1.0 1.0)))

(define (rotate180-alt painter)
  (transform-painter
   painter
   (vect 1.0 1.0)
   (vect 0.0 1.0)
   (vect 1.0 0.0)))
(define (rotate270-alt painter)
  (transform-painter
   painter
   (vect 0.0 1.0)
   (vect 0.0 0.0)
   (vect 1.0 1.0)))

; EX 2.51
; define the below operation which takes two painters as arguments and produces a procedure that, given a frame, draws the first painter in the bottom of the frame
; and the second painter on top. define below in two different ways- first by writing a procedure that is analgous to the beside procedure given in the book:
(define (below-alt1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
; now write the procedure in terms of beside and suitable rotation operations
(define (below-alt2 painter1 painter2)
  (let ((bottom-painter (rotate270 painter1))
        (top-painter (rotate270 painter2)))
    (rotate90 (beside bottom-painter top-painter))))

; EX 2.52
; make changes to the square limit of wave shown in figure 2.9 by working at each level of the language
; a. add some segments to the primitive mark-of-zorro painter
; for solution to 2.52a, see 2.49d

; b. change the pattern constructed by corner-split (for example, use only one copy of the up-split and right-split images instead of two)
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; actual solution to 2.52b, the rest are supporting functions
(define (corner-split-alt painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-limit-altb painter n)
  (let ((quarter (corner-split-alt painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 1))
(paint (square-limit-altb einstein 1))

; modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (for example, flip the corner copies of the
; painter)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit-sof painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-limit-sof-alt painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                      identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (square-limit-sof-alt einstein 1))