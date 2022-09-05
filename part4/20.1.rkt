;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a DT is a structure with the following fields
; - name (symbol)
; - size (number or DIR)
; - child

; child is one of:
; - '()
; - [List-of DT]

(define-struct dt [name size child])

; EX 329

(define EXAMPLE-DT (make-dt 'TS 'DIR
                              (list (make-dt 'TEXT 'DIR
                                               (list (make-dt 'part1 99 '()) (make-dt 'part2 52 '()) (make-dt 'part3 17 '())))
                                    (make-dt 'read! 10 '())
                                    (make-dt 'Libs 'DIR
                                               (list (make-dt 'Docs 'DIR
                                                                (list (make-dt 'read! 19 '())))
                                                     (make-dt 'Code 'DIR
                                                                (list (make-dt 'hang 8 '()) (make-dt 'draw 2 '()))))))))

; DT Symbol -> Number
; counts the number of the times the given name is used in the given DT
(define (count adt symb)
  (+ (if (equal? (dt-name adt) symb) 1 0)
     (foldl + 0 (map (lambda (x) (count x symb)) (dt-child adt)))))
  ;(local (
  ;        (define (process-head adt)
  ;            (if (equal? (dt-name adt) symb) 1 0))
;
;          ; [List-of DT] symbol -> Number
;          (define (process-child loc)
;            ;(cond
;            ;  [(empty? loc) 0]
;            ;  [else (+ (count (first loc) symb) (process-child (rest loc)))])))
;      **v2   (foldl + 0 (map (lambda (x) (count x symb)) loc))))
;
;  (+ (process-head adt) (process-child (dt-child adt)))))


(check-expect (count EXAMPLE-DT 'read!) 2)

; DT DT -> [List-of Symbol]
; returns the path from the first DT to the second given DT if it is a child of the first
; #false if it is not
(define (path root in-root)
  root)

; DT DT -> Boolean
; returns true if the second given DT is a child of the first
; false if it is not
(define (member-dt? root adt)
   (member-child? (dt-child root) adt))

(define (check-dt adt target-dt)
  (equal? target-dt adt))

(define (member-child? loc adt)
  (cond
    [(empty? loc) #false]
    [else (cond
            [(equal? adt (first loc)) #true]
            [else (or (member-child? (first (dt-child loc)) adt)
                      (member-dt? (first loc) adt))])]))
  

(check-expect (member-dt? EXAMPLE-DT (make-dt 'Docs 'DIR
                                                    (list (make-dt 'read! 19 '())))) #true)
