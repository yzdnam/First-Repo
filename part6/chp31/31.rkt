;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |31|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))

; EX 489
; reformulate add-to-each using map and lambda
(define (add-to-each-abs n l)
  (map (lambda (x) (+ n x)) l))

(check-expect (cons 50 (add-to-each-abs 50 '(40 110 140 170)))
              '(50 90 160 190 220))

; EX 490
; on paper

; EX 491 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (local ((define (move-to-end i l)
                    (cond
                      [(empty? (rest l)) (list i)]
                      [else (move-to-end i (rest l))])))
            (append (rev (rest l)) (move-to-end (first l) l) ))]))

(check-expect (rev (list 1 2 3)) (list 3 2 1))

; EX 492 - modify the definition of find-path so that it exploits an accumulator to produce #false if it encounters an origin it has already inspected
; testing sample-graph on B' D' or C' D' will produce an infinite loop until this exercise is completed

; produces the list of immediate neighbors of n in g
(define (neighbors n g)
  (cond
    [(empty? g) (error "Node name is not in the given graph. Terminating.")]
    [else (local ((define junction (first g)))
            (if (symbol=? (first junction) n)
                (second junction)
                (neighbors n (rest g))))]))

            ; Node Node Graph -> [Maybe Path]
            ; finds a path from origination to destination in G
            ; if there is no path, the function produces #false
            (define (find-path origination destination G)
              (local (; Node Node Graph [List-of Node] -> [Maybe Path]
                      (define (find-path/a origination seen)
                        (cond
                          [(symbol=? origination destination) (list destination)]
                          [(member? origination seen) #f]
                          [else (local ((define next (neighbors origination G))
                                        (define candidate
                                          (local (
                                                  ; [List-of Node] Node Graph -> [Maybe Path]
                                                  ; finds a path from some node on lo-Os to D
                                                  ; if there is no path, the function produces #false
                                                  (define (find-path/list lo-Os seen)
                                                    (cond
                                                      [(empty? lo-Os) #false]
                                                      [else (local ((define candidate
                                                                      (find-path/a (first lo-Os) seen)))
                                                              (cond
                                                                [(boolean? candidate)
                                                                 (find-path/list (rest lo-Os) (cons (first lo-Os) seen))]
                                                                [else candidate]))])))
                                            (find-path/list next (cons origination seen)))))
                                  (cond
                                    [(boolean? candidate) #false]
                                    [else (cons origination candidate)]))])))
                (find-path/a origination '())))
             

    (define sample-graph
      '((A (B E))
        (B (E F))
        (C (B D))
        (D ())
        (E (C F))
        (F (D G))
        (G ())))


(check-expect (find-path 'C 'D sample-graph)
              '(C B E F D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
