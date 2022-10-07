;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EX 485
; a number-tree is one of the following
; - Number
; (list number-tree number-tree)

; number-tree -> Number
; determines the sum of the numbers in a number-tree
(define (sum-tree ntree)
  (cond
    [(number? ntree) ntree]
    [else (+ (sum-tree (first ntree)) (sum-tree (second ntree)))]))

(check-expect (sum-tree (list (list 1 (list 1 1)) 1)) 4)

(define (searchS x l)
  (cond
    [(= (length l) 0) #false]
    [else
     (or (= (first l) x)
         (searchS
           x (rest l)))]))

(define (searchL x l)
  (cond
    [(empty? l) #false]
    [else
     (or (= (first l) x)
         (searchL
           x (rest l)))]))

; N -> [List Number Number]
; how long do searchS and searchL take 
; to look for n in (list 0 ... (- n 1))
(define (timing n)
  (local ((define long-list
            (build-list n (lambda (x) x))))
    (list
      (time (searchS n long-list))
      (time (searchL n long-list)))))