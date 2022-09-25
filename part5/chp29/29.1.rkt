;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |29.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sample-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

(define sample-graph-w-all-paths
  (list (list 'A (list 'B 'E))
        (list 'B (list 'C 'E 'F))
        (list 'C (list 'D 'F 'G))
        (list 'D (list 'G))
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

; A Node is a Symbol
; A Paths-List is a [List-of Node]:
; A Junction is a list whose first member is a Node and whose second member is a Paths-List that does not include the first Node listed in the Junction
; A Graph is a [List-of Junction]

; EX 471
; Node Graph -> Paths-List
; produces the list of immediate neighbors of n in g
(define (neighbors n g)
  (cond
    [(empty? g) (error "Node name is not in the given graph. Terminating.")]
    [else (local ((define junction (first g)))
            (if (symbol=? (first junction) n)
                (second junction)
                (neighbors n (rest g))))]))

(check-expect (neighbors 'F sample-graph) (list 'D 'G))

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
 
(define (find-path origination destination G)
 (cond
    [(symbol=? origination destination)
     (list destination)]
    [else
     (local ((define next (neighbors origination G))
             (define candidate
               (find-path/list next destination G)))
       (cond
         [(boolean? candidate) #false]
         [else (cons origination candidate)]))]))

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-originations to
; destination; otherwise, it produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; EX 472 - test find-path on inputs 'A 'G and sample-graph. 'A and 'G have three possible paths between each other. the path that is returned will be the one where each node is
; the earliest occurence of a viable path within each origination's list of paths.
;(check-expect (find-path 'A 'G sample-graph) (list 'A 'B 'E 'F 'G))

; Graph -> Boolean
; determines whether there is a path between every pair of nodes in a graph
(define (test-on-all-nodes g)
  (local ((define testing-node (first (first g)))
          (define other-junctions (rest g))
          ; Node Graph -> Boolean
          (define (test-ea-node node paths testing-graph)
            (cond
              [(and (empty? paths) (empty? (rest (rest testing-graph)))) #true]
              [(empty? paths) (test-ea-node (first (second testing-graph)) (rest (rest testing-graph)) (rest testing-graph))]
              [else (local ((define test-junction (first (first paths))))
                      (cond
                        [(or (cons? (find-path test-junction node g))
                             (cons? (find-path node test-junction g))) ; checks both directions
                         (test-ea-node node (rest paths) testing-graph)]
                        [else #false]))])))
    (test-ea-node testing-node other-junctions g)))

(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes sample-graph-w-all-paths) #true)

(define cyclic-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'B 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

; EX 473 - (find-path 'B 'C cyclic-graph) terminates. (test-on-all-nodes cyclic-graph) does not terminate.

; EX 474 - redesign find-path as a single-program
; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

; EX 475 - redesign find-path/list so that it uses an existing list abstraction provided by Racket

(check-expect (find-path.474 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path.474 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path.474 'C 'G sample-graph)
              #false)
 
(define (find-path.474 origination destination G)
 (cond
    [(symbol=? origination destination)
     (list destination)]
    [else
     (local ((define next
               (local (; Node Graph -> Paths-List
                       ; produces the list of immediate neighbors of n in g
                       (define (neighbors.474 n g)
                         (cond
                           [(empty? g) (error "Node name is not in the given graph. Terminating.")]
                           [else (local ((define junction (first g)))
                                   (if (symbol=? (first junction) n)
                                       (second junction)
                                       (neighbors.474 n (rest g))))])))
                 (neighbors.474 origination G)))
             (define candidate
               (local (; [List-of Node] Node Graph -> [Maybe Path]
                       ; finds a path from some node on lo-originations to
                       ; destination; otherwise, it produces #false
                       (define (find-path/list.474 lo-Os D G)
                         (local ((define possible-paths (filter (lambda (x) (not (boolean? x))) (map (lambda (x) (find-path.474 x D G)) lo-Os))))
                           (cond
                             [(empty? possible-paths) #false]
                             [else (first possible-paths)]))))
                         ;(cond
                         ;  [(empty? lo-Os) #false]
                         ;  [else (local ((define candidate
                         ;                  (find-path.474 (first lo-Os) D G)))
                         ;          (cond
                         ;            [(boolean? candidate)
                         ;             (find-path/list.474 (rest lo-Os) D G)]
                         ;            [else candidate]))])))
                 (find-path/list.474 next destination G))))
       (cond
         [(boolean? candidate) #false]
         [else (cons origination candidate)]))]))

; EX 476 - design fsm-match which consumes an FSM and a string and produces #true if the sequence of characters in the string cause the FSM to transition from an initial state
; to a final state

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(define (fsm-match? an-fsm a-string)
  (cond
    [(equal? (fsm-initial an-fsm) (fsm-final an-fsm)) #true]
    [else (local ((define key-strokes (explode a-string))
                  ; 1Transitition FSM -> FSM
                  (define (check-key key an-fsm)
                    (local ((define trans-list (fsm-transitions an-fsm))
                            (define eligible-trans (filter (lambda (x) (string=? (transition-current x) (fsm-initial an-fsm))) trans-list))
                            (define key-matches (filter (lambda (x) (string=? key (transition-key x))) eligible-trans)))
                      (if (empty? key-matches)
                          an-fsm
                          (make-fsm (transition-next (first key-matches)) (fsm-transitions an-fsm) (fsm-final an-fsm))))))
            (cond
              [(empty? key-strokes) #false]
              [else (fsm-match? (check-key (first key-strokes) an-fsm) (implode (rest key-strokes)))]))]))

(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))

; EX 477 - explain the design of the generative-recursive version of arrangements. answer all questions the design recipe for generative recursion poses including termination
; 1. problem analysis: the function needs to create a list of all rearrangements of the items in a list of arbitrary items
; 2. header: the problem analysis already explains what the function intends to do. the function completes this by recursing on the given list minus one of its items
; until it is dealing with an empty list. each time the function recurses it adds the item removed for the future iteration back to the front of the list of each arrangement in
; existence at the time of the cycle. the function will complete this cycle starting with each item using foldr.
; 3. the list of letters, "r" "a" "t", was used as an example and test
; 4. the generative recursion template of breaking down the problem was used. specifically, the trivial problem of an empty list of lists was identified and solved for,
; the generation of new problems for nontrivial problems is completed by calling up arrangements for the given list minus an item. once a cycle of arrangements is completed on the
; list starting at the first item of the list, it is recalled starting on the second item of the list and so on. the solutions are combined using append.
; 5. the function terminates because it iterates on versions smaller than itself within each step of foldr and foldr terminates once it has reached the end of the list it has been
; called on