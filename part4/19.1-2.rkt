;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct person [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))

; Oldest Generation:
(define Carl (make-person NP NP "Carl" 1926 "green"))
(define Bettina (make-person NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-person Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-person Carl Bettina "Dave" 1955 "black"))
(define Eva (make-person Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-person NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-person Fred Eva "Gustav" 1988 "brown"))

; FT Function Template
; FT -> ???
(define (fun-FT an-ftree)
  (cond
    [(no-parent? an-ftree) ...]
    [else (... (fun-FT (person-father an-ftree)) ...
           ... (fun-FT (person-mother an-ftree)) ...
           ... (person-name an-ftree) ...
           ... (person-date an-ftree) ...
           ... (person-eyes an-ftree) ...)]))

; EXAMPLE FUNCTION
; FT -> Boolean
; does an-ftree contain a person
; structure with "blue" in the eyes field
 
(check-expect (blue-eyed-person? Carl) #false)
(check-expect (blue-eyed-person? Gustav) #true)
 
(define (blue-eyed-person? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (person-eyes an-ftree) "blue")
              (blue-eyed-person? (person-father an-ftree))
              (blue-eyed-person? (person-mother an-ftree)))]))

; EX 310
; FT -> Number
; counts the child structures in the tree
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (count-persons (person-father an-ftree)) (count-persons (person-mother an-ftree)) 1)]))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Eva) 3)
(check-expect (count-persons Gustav) 5)

; EX 311
; FT number -> number
; consumes an FT and the current year
; produces the average age of all child structures in the family tree
(define (average-age an-ftree current-year)
  (cond
    [(no-parent? an-ftree) 0]
    [else (/ (+ (* (average-age (person-father an-ftree) current-year) (count-persons (person-father an-ftree)))
                (* (average-age (person-mother an-ftree) current-year) (count-persons (person-mother an-ftree)))
                (- current-year (person-date an-ftree)))
             (count-persons an-ftree))]))

(check-expect (average-age Carl 2022) 96)
(check-expect (average-age Eva 2022) 83)
(check-expect (average-age Gustav 2022) 67.8)

; EX 312
; FT -> list-of strings
(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (cons (person-eyes an-ftree)
                 (append
                   (eye-colors (person-father an-ftree))
                   (eye-colors (person-mother an-ftree))))]))

(check-expect (eye-colors Carl) (list "green"))

; EX 313
; FT -> Boolean
; produces #true only when an ancestor has blue eyes,
; not the given child itself
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or
       (blue-eyed-person?
         (person-father an-ftree))
       (blue-eyed-person?
         (person-mother an-ftree)))]))

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

; An FF is a [List-of FTs]

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; FF -> Boolean
; does the forest contain any child with "blue" eyes
 
(check-expect (blue-eyed-person-in-forest? ff1) #false)
(check-expect (blue-eyed-person-in-forest? ff2) #true)
(check-expect (blue-eyed-person-in-forest? ff3) #true)
 
(define (blue-eyed-person-in-forest? a-forest)
  (ormap blue-eyed-person? a-forest))

; FF -> Number
; returns number of persons in a FF
(define (count-persons-forest an-forest)
  (cond
    [(empty? an-forest) 0]
    [else (+ (count-persons (first an-forest))
             (count-persons-forest (rest an-forest)))]))

; FF Number -> Number
; returns the average age of the person-instances in the FF
(define (average-age-forest an-forest current-year)
  (cond
    [(empty? an-forest) 0]
    [else (/ (+ (* (average-age-forest (rest an-forest) current-year) (count-persons-forest (rest an-forest)))
             (* (average-age (first an-forest) current-year) (count-persons (first an-forest))))
          (count-persons-forest an-forest))]))

(check-expect (average-age-forest ff1 2022) 96)
(check-expect (average-age-forest ff2 2022) 76.25)
(check-expect (average-age-forest ff3 2022) 80.2)