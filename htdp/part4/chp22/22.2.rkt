;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |22.2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; An Xexpr.v2 is a list whose first item is a Symbol followed by one of the following:
; - Xexpr.v2
; - Attributes
; - XWord

; Attributes are one of the following:
; -'()
; - (cons Symbol (cons String Attributes))
; - (cons Attributes Xexpr.v2)

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; [List-of Attribute] or Xexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> Symbol
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> Symbol
; retrieves the content of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))

; An Xexpr.v2-Body is one of the following:
; -[List-of Attribute]
; -Xexpr.v2

; [List-of Attribute] Symbol -> [Maybe- String]
; if the attributes list associates the symbol with a string, the function retrieves the string; otherwise it returns #false
(define (find-attr loa symb)
  (if (list? (assq symb loa)) (second (assq symb loa))
      #false))

; An XWord is '(word ((text String))).

; EX 370
(define xword1 '(word ((text "one"))))
(define xword2 '(word ((text "two"))))
(define xword3 '(word ((text "three"))))

; [ISL+ Value] XWord -> Boolean
; checks whether some ISL+ value is in XWord
(define (word? val)
  (symbol=? 'word (first val)))

(check-expect (word? xword1) #true)
(check-expect (word? xword2) #true)

; XWord -> String
; extracts the value of the only attribute of an instance of XWord
(define (word-text xwrd)
  (second (first (xexpr-attr xwrd))))

(check-expect (word-text xword1) "one")
(check-expect (word-text xword2) "two")

; EX 371 is the addition of XWords to the data definition at the top of the program

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid COLOR) (text " " SIZE COLOR)))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define i1
  '(li (word ((text "one")))))
(define i2
  '(li (word ((text "two")))))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet 
(define (render-item1 i)
   (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; EX 372
(check-expect (render-item1 '(li (word ((text "two"))))) (beside/align 'center BT
                                                                       (text "two" 12 'black)))
; render-item1 first extracts the contents of the XItem using the xexpr-content function, then extracts the XWord using first,
; the string is extracted from the XWord using the word-text function, then the string is formatted to be rendered as an image
; and passed to beside/align to render the rendered string next to a bullet

; XEnum.v1 -> Image 
; renders a simple enumeration as an image 
(check-expect (render-enum1 e0) e0-rendered)
(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left
                          (render-item1 item)
                          so-far)))
    (foldr deal-with-one empty-image content)))

; XItem and XEnum Refinements to cope with arbitrarily nested enumerations
; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; Refined functions to match refinements of data definitions
; EX 373 Test cases for the refined functions

; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

(check-expect (render-enum e0) (above/align 'left (render-item i1) (above/align 'left (render-item i2) empty-image)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE COLOR)]
        [else (render-enum content)]))))

(check-expect (render-item i1) (bulletize (text "one" SIZE COLOR)))

; EX 374 XEnum.v2 and XItem.v2 data definitions and rendering functions using cons

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord XItem.v2)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 XItem.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; XEnum.v2 -> Image
; renders an XEnum.v2
(define (render-xenum xenum)
  (local ((define xenum-content (xexpr-content xenum))
          (define (deal-with-one item so-far)
            (above/align 'left (render-xitem item) so-far)))
    (foldr deal-with-one empty-image xenum-content)))

(check-expect (render-xenum e0) (above/align 'left (render-item i1) (above/align 'left (render-item i2) empty-image)))

; XItem.v2 -> Image
; renders an XItem.v2
(define (render-xitem xitem)
  (local ((define xitem-content (first (xexpr-content xitem))))
    (bulletize
      (cond
        [(word? xitem-content)
         (text (word-text xitem-content) SIZE COLOR)]
        [else (render-xenum xitem-content)]))))

(check-expect (render-xitem i1) (bulletize (text "one" SIZE COLOR)))

; EX 375 alternate render-item with bulletize appearing in each clause
; XItem.v2 -> Image
; renders an XItem.v2
(define (render-xitem-alt xitem)
  (local ((define xitem-content (first (xexpr-content xitem))))
      (cond
        [(word? xitem-content)
         (bulletize (text (word-text xitem-content) SIZE COLOR))]
        [else (bulletize (render-xenum xitem-content))])))

; EX 376
; XEnum.v2 -> Number
; counts all "hello"s in an instance of XEnum.v2
(define (enum-hello-count xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (+ (hello? item) so-far)))
    (foldr deal-with-one 0 content)))

(check-expect (enum-hello-count e1) 2)
(check-expect (enum-hello-count e2) 1)

; XItem.v2 -> Number
; returns 1 if XItem.v2 is an XWord containing "hello", 0 otherwise 
(define (hello? an-item)
  (local ((define content (first (xexpr-content an-item))))
      (cond
        [(word? content)
         (if (string=? "hello" (word-text content)) 1 0)]
        [else (enum-hello-count content)])))

(check-expect (hello? i1) 0)
(check-expect (hello? i3) 1)

(define e1
  '(ul
    (li (word ((text "hello"))))
    (li (word ((text "hello"))))))

(define e2
  '(ul
    (li (word ((text "lo"))))
    (li (word ((text "hello"))))))

(define i3
  '(li (word ((text "hello")))))

; EX 377
; XEnum.v2 -> XEnum.v2
; replaces all "hello"s in an instance of XEnum.v2 with "bye
(define (enum-hello-bye xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (cons (hello?-bye item) so-far)))
    (cons 'ul (foldr deal-with-one '() content))))

(check-expect (enum-hello-bye e1)
  '(ul
    (li (word ((text "bye"))))
    (li (word ((text "bye"))))))
              
(check-expect (enum-hello-bye e2) 
  '(ul
    (li (word ((text "lo"))))
    (li (word ((text "bye"))))))

; XItem.v2 -> XItem.v2
; replaces XItem.v2 word string with "bye" if it is an XWord containing "hello" 
(define (hello?-bye an-item)
  (local ((define content (first (xexpr-content an-item))))
      (cond
        [(word? content)
         (if (string=? "hello" (word-text content)) '(li (word ((text "bye"))))
             an-item)]
        [else (enum-hello-bye content)])))

(check-expect (hello?-bye i1) i1)
(check-expect (hello?-bye i3) '(li (word ((text "bye")))))