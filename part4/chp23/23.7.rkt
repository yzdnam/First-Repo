;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23.7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

; EX 403 - use a structure type for Specs
(define-struct spec [label predicate])

(define school-schema (list (make-spec "Name" string?)
                            (make-spec "Age" number?)
                            (make-spec "Present" boolean?)))

(define school-content '(("Alice" 35 #true)
                         ("Bob" 25 #false)
                         ("Carol" 30 #true)
                         ("Dave" 32 #false)))

(define presence-schema (list (make-spec "Present" boolean?)
                              (make-spec "Description" string?)))

(define presence-content '((#true "presence")
                           (#false "absence")))

(define school-presence-content '(("Alice" 35 #true "presence")
                                  ("Bob" 25 #false "absence")
                                  ("Carol" 30 #true "presence")
                                  ("Dave" 32 #false "absence")))

(define school-presence-schema (list (make-spec "Name" string?)
                                      (make-spec "Age" number?)
                                      (make-spec "Present" boolean?)
                                      (make-spec "Description" string?)))


(define school-presence-db (make-db school-presence-schema school-presence-content))
(define school-db (make-db school-schema school-content))
(define presence-db (make-db presence-schema presence-content))
(define fail-db (make-db presence-schema school-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check fail-db) #false)
 
(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define width (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    width)
                 (andmap (lambda (s c) [(spec-predicate s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))
  
; EX 404
; [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; applies the given function to pairs of corresponding values from the two lists and if f always produces #true
; andmap2 produces #true too. Assumption is that the given lists are equally long
(define (andmap2 func l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) #true]
    [(func (first l1) (first l2)) (andmap2 (rest l1) (rest l2))]
    [else #false]))

; Expression hoisting is defining an extraction of data that remains the same to make the function run faster, see schema, content, and width in integrity-check

; EX 405 - design row-filter in project function
; DB [List-of Label] -> DB
; retains a column from db if its label is in labels

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))

(check-expect
  (db-content (project school-db '("Name" "Present")))
  projected-content)

(check-expect
  (db-content (project.v2 school-db '("Name" "Present")))
  projected-content)

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (spec-label c) labels))
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (local ((define schema-labels (map spec-label schema)) ; EX 406 expression hoisted out of row-project's interaction's area because the result does not differ from
                                                                   ; function call to function call
                    ; Row [List-of Label] -> Row
                    ; retains those cells whose corresponding element 
                    ; in names is also in labels
                    (define (row-filter row names)
                      (foldr (lambda (x y z) (cond ; EX 407 row-filter redesigned using foldr
                                             [(member? x labels) (cons y z)]
                                             [else z])) '() names row)))
                      ;(cond
                      ;  [(empty? row) '()]
                      ;  [(member? (first names) labels) (cons (first row) (row-filter (rest row) (rest names)))]
                      ;  [else (row-filter (rest row) (rest names))])))
              (row-filter row schema-labels))))
    (make-db (filter keep? schema)
             (map row-project content))))

(define (project.v2 db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (spec-label c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema))) ; mask is an expression hoist of (member? x labels) from the project.v1 function
    (make-db (filter keep? schema)
             (map row-project content))))

; EX 408
; DB [List-of Label] [X -> Boolean] -> [List-of Row]
; results in a list of rows that satisfy the given predicate, projected down to the given set of labels
(define (select adb lo-label pred)
  (local ((define content (db-content adb))
          (define filtered-db (make-db (db-schema adb) (filter pred content))))
    (db-content (project.v2 filtered-db lo-label))))

(check-expect (select school-presence-db (list "Name" "Present") (lambda (x) (<= (second x) 30))) '(("Bob" #false)
                                                                                                    ("Carol" #true)))

; EX 409
; DB [List-of Label] -> DB
; produces a database with the same content as the given database but with its columns reordered according to the given list of labels
(define (reorder adb lol)
  (local ((define orig-schema (db-schema adb))
          (define content (db-content adb))
          ; filters the schema for each label in order of the labels listed in the given label list, then folds the schema into a single list 
          (define (reorder-schema schema)
            (foldr (lambda (x y) (cond
                                   [(empty? x) y]
                                   [else (cons (first x) y)])) '()
                   (map (lambda (label) (filter (lambda (x) (equal? (spec-label x) label)) schema)) lol)))
          (define new-schema (reorder-schema orig-schema))
          (define (reorder-row row old-schema new-schema)
            (local (; returns index of old-schema items within the original schema list
                    (define (find-index old-schema N 1new-schema)
                      (if (equal? (first old-schema) 1new-schema) N
                          (find-index (rest old-schema) (add1 N) 1new-schema)))
                    ; creates a list of the original schema indices in the places they will assume in the new schema
                    (define list-refs-for-new-schema (map (lambda (x) (find-index old-schema 0 x)) new-schema)))
              ; applies the list of original schema indices to an arbitrary row to create the row according to the new-schema
              (map (lambda (lref) (list-ref row lref)) list-refs-for-new-schema)))
          (define new-content (map (lambda (row) (reorder-row row orig-schema new-schema)) content)))
    (make-db new-schema new-content)))

(check-expect (db-content (reorder presence-db (list "Description" "Present"))) '(("presence" #true) ("absence" #false)))

(check-expect (db-content (reorder presence-db (list "Description"))) '(("presence") ("absence")))

(check-expect (db-content (reorder presence-db (list "Description" "Test_Label"))) '(("presence") ("absence")))

; tests for helper functions
;(check-expect (map (lambda (x) (spec-label x)) (reorder-schema (list (make-spec "Present" boolean?) (make-spec "Description" string?)) (list "Description" "Present")))
;                              (list "Description" "Present"))

;(check-expect (map (lambda (x) (spec-label x)) (reorder-schema school-presence-schema (list "Description" "Age" "Name" "Present")))
;              (list "Description" "Age" "Name" "Present"))

;(check-expect (reorder-row (list "Alice" 35 #true "presence") (list (make-spec "Name" string?)
;                                                                    (make-spec "Age" number?)
;                                                                    (make-spec "Present" boolean?)
;                                                                    (make-spec "Description" string?)) (list (make-spec "Age" number?)
;                                                                                                             (make-spec "Present" boolean?)
;                                                                                                             (make-spec "Name" string?)
;                                                                                                             (make-spec "Description" string?)))
;                                                                                                                                                (list 35 #true "Alice" "presence"))

; EX 410
; DB DB -> DB
; consumes two DBs with the exact same schema and produces a new DB with this schema and the joint content of both. Eliminates duplicate rows.
(define (db-union db1 db2)
  (local ((define schema (db-schema db1))
          (define db1-content (db-content db1))
          (define db2-content (db-content db2))
          (define (combine-content con1 con2)
            (cond
              [(empty? con1) con2]
              [else (cons (first con1) (combine-content (rest con1) con2))]))
          (define combined-content (combine-content db1-content db2-content))
          (define (eliminate-dupes li)
            (cond
              [(empty? li) li]
              [(member? (first li) (rest li)) (eliminate-dupes (rest li))]
              [else (cons (first li) (eliminate-dupes (rest li)))]))
          (define clean-new-content (eliminate-dupes combined-content)))
    (make-db schema clean-new-content)))

(check-expect (db-content (db-union school-db (make-db school-schema '(("Alice" 35 #true)
                                                                       ("Mark" 92 #true)
                                                                       ("Alonzo" 45 #false))))) '(("Bob" 25 #false)
                                                                                                  ("Carol" 30 #true)
                                                                                                  ("Dave" 32 #false)
                                                                                                  ("Alice" 35 #true)
                                                                                                  ("Mark" 92 #true)
                                                                                                  ("Alonzo" 45 #false)))

; EX 411
; DB DB -> DB
; Assumption is that the schema of db-1 ends in the same spec as the first spec for the schema of db2.
; The function creates a database from db-1 by replacing the last cell in each row with the translation of the cell in db-2
(define (join db1 db2)
  (local (; defs and funcs to join schema
          (define base-schema (db-schema db1))
          (define translation-schema (db-schema db2))
          ; list list -> list
          ; replace last element in a list with all elements in li2 except the first one
          (define (join-lists li1 li2)
            (local ((define rest-li1 (rest li1)))
            (cond
              [(empty? rest-li1) (rest li2)]
              [else (cons (first li1) (join-lists rest-li1 li2))])))

          (define new-schema (join-lists base-schema translation-schema))

          ; defs and funcs to join content
          (define base-content (db-content db1))
          (define translation-content (db-content db2))
          (define (my-last li)
            (cond
              [(empty? (rest li)) (first li)]
              [else (my-last (rest li))]))
          ; list list -> [list-of list]
          (define (check-row-and-join-if-good row1 translation-content)
              (cond
                [(empty? translation-content) '()]
                [else (local ((define translation-row-to-be-checked (first translation-content))
                              (define rest-of-translation-content (rest translation-content)))
                        (cond
                          [(equal? (my-last row1) (first translation-row-to-be-checked))
                           (cons (join-lists row1 translation-row-to-be-checked) (check-row-and-join-if-good row1 rest-of-translation-content))]
                          [else (check-row-and-join-if-good row1 rest-of-translation-content)]))]))
          
          (define new-content
            (foldr (lambda (row x) (append (check-row-and-join-if-good row translation-content) x)) '() base-content)))
    
    (make-db new-schema new-content)))

(check-expect (db-content (join school-db presence-db)) '(("Alice" 35 "presence")("Bob" 25 "absence")("Carol" 30 "presence")("Dave" 32 "absence")))

(check-expect (db-content (join school-db (make-db presence-schema '((#true "presence")(#true "here")(#false "absence")(#false "there")))))
              '(("Alice" 35 "presence")("Alice" 35 "here")("Bob" 25 "absence")("Bob" 25 "there")("Carol" 30 "presence")("Carol" 30 "here")("Dave" 32 "absence")("Dave" 32 "there")))       

; helper function test
; (check-expect (map (lambda (x) (spec-label x)) (join-lists school-schema presence-schema)) (list "Name" "Age" "Description"))