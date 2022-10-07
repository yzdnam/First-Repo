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

;;;
; DT Symbol -> Number
; counts the number of the times the given name is used in the given DT
(define (count adt symb)
  (+ (if (equal? (dt-name adt) symb) 1 0)
     (foldl + 0 (map (lambda (x) (count x symb)) (dt-child adt)))))
  ;(local (; process the head of a node
  ;        (define (process-head adt)
  ;            (if (equal? (dt-name adt) symb) 1 0))
;
;          ; [List-of DT] symbol -> Number
           ; process the child of a node
;          (define (process-child loc)
;            ;(cond
;            ;  [(empty? loc) 0]
;            ;  [else (+ (count (first loc) symb) (process-child (rest loc)))])))
;      **v2   (foldl + 0 (map (lambda (x) (count x symb)) loc))))
;
;  (+ (process-head adt) (process-child (dt-child adt)))))


(check-expect (count EXAMPLE-DT 'read!) 2)

;;;
; DT DT -> [List-of Symbol]
; returns the path from the first DT to the second given DT if it is a child of the first
; #false if it is not
(define (path root in-root)
  (cond
    [(equal? root in-root) (list (dt-name root))]
    [else (if (member-dt? root in-root)
              (cons (dt-name root) (local (
                                           (define in-path-directory
                                             (first (filter (lambda (x) (member-dt? x in-root)) (dt-child root)))))
                                     (path in-path-directory in-root)))
              #false)]))
;  (local ( ; DT [List-of Symbol] -> [List-of Symbol]
           ; add the name of the DT to the path list
;           (define (add-to-path adt pathlist)
;             (cons (dt-name adt) pathlist))

           ; [List-of DT] DT -> [List-of Symbol]
;           (define (find-path loc)
           ;  (cond
           ;    [(empty? loc) #false]
           ;    [else (if (member-dt? (first loc) in-root)
           ;              (path (first loc) in-root)
           ;              (find-path (rest loc) in-root))]))
;             (local (
;                     (define in-path-directory (first (filter (lambda (x) (member-dt? x in-root)) loc))))
;               (path in-path-directory in-root))))
          
;    (cond
;      [(equal? root in-root) (list (dt-name root))]
;      [else (if (member-dt? root in-root)
;                (add-to-path root (find-path (dt-child root)))
;                #false)])))
       

(check-expect (path EXAMPLE-DT (make-dt 'Docs 'DIR
                                              (list (make-dt 'read! 19 '()))))
              (list 'TS 'Libs 'Docs))

; DT DT -> Boolean
; returns true if the second given DT is a child of the first
; false if it is not
(define (member-dt? root adt)
  (or
   (equal? root adt)
   (ormap (lambda (x) (member-dt? x adt)) (dt-child root))))
;  (local (
;          (define (check-dt checked-dt adt)
;            (equal? checked-dt adt))

;          (define (member-child? loc adt)
;            ;(cond
;            ;  [(empty? loc) #false]
;            ;  [else (or
;            ;          (member-dt? (first loc) adt)
;            ;          (member-child? (rest loc) adt))])))
;       **v2  (ormap (lambda (x) (member-dt? x adt)) loc)))
;          
;    (or
;      (check-dt root adt)
;      (member-child? (dt-child root) adt))))
  

(check-expect (member-dt? EXAMPLE-DT (make-dt 'Docs 'DIR
                                                    (list (make-dt 'read! 19 '())))) #true)

;;;
; DT -> Number
; total size of all files in the tree if directories have no size
(define (tree-size adt)
  (+ (if (equal? (dt-size adt) 'DIR) 0 (dt-size adt))
     (foldl + 0 (map (lambda (x) (tree-size x)) (dt-child adt)))))
;  (+ (file-size adt) (find-files (dt-child adt))))

(define (file-size file)
  (cond
    [(equal? (dt-size file) 'DIR) 0]
    [else (dt-size file)]))

(define (find-files loc)
  (cond
    [(empty? loc) 0]
    [else (+ (tree-size (first loc)) (find-files (rest loc)))]))
;  (foldl + 0 (map (lambda (x) (tree-size x)) loc)))
    
(check-expect (tree-size EXAMPLE-DT) (+ 99 52 17 10 19 8 2))

;;;
; DT -> Number
; total size of all files in the tree if directories have a size of 1
(define (tree-size-alt adt)
  (+ (if (equal? (dt-size adt) 'DIR) 1 (dt-size adt))
     (foldl + 0 (map (lambda (x) (tree-size-alt x)) (dt-child adt)))))

(check-expect (tree-size-alt EXAMPLE-DT) (+ 5 99 52 17 10 19 8 2))

;;;
; DT -> Number
; returns number of levels the DT contains
(define (depth adt)
  (cond
    [(>= (check-depth adt) (check-childs (dt-child adt))) (check-depth adt)]
    [else (check-childs (dt-child adt))]))

(define (check-depth adt)
  (if (not (empty? (dt-child adt))) (+ 1 (check-childs (dt-child adt))) 0))

(define (check-childs loc)
  (cond
    [(empty? loc) 0]
;    [else (cond
;            [(>= (depth (first loc)) (check-childs (rest loc))) (depth (first loc))]
;            [else (check-childs (rest loc))])]))
    [else (first (sort (map (lambda (x) (depth x)) loc) >))]))

(check-expect (depth EXAMPLE-DT) 3)