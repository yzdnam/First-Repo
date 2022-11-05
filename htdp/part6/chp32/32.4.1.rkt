;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32.4.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
(check-expect
 (add-at-end '() "a")
 (cons "a" '()))
 
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end (rest l) s))]))

; EX 177
; string string -> Editor

(check-expect (create-editor "all" "good") (make-editor (cons "l" (cons "l" (cons "a" '()))) (cons "g" (cons "o" (cons "o" (cons "d" '()))))))

(define (create-editor string1 string2)
  (make-editor (rev (explode string1)) (explode string2)))

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e)
 (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))

; Lo1s -> Image
; renders a list of 1Strings as a text image 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))


; Editor KeyEvent -> Editor
; deals with a key event, given some editor

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))

(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "\b")
  (create-editor "c" "fgh"))
(check-expect
  (editor-kh (create-editor "" "fgh") "\b")
  (create-editor "" "fgh"))
(check-expect
  (editor-kh (create-editor "fgh" "") "\b")
  (create-editor "fg" ""))

(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "left")
  (create-editor "c" "dfgh"))
(check-expect
  (editor-kh (create-editor "" "fgh") "left")
  (create-editor "" "fgh"))
(check-expect
  (editor-kh (create-editor "fgh" "") "left")
  (create-editor "fg" "h"))

(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "right")
  (create-editor "cdf" "gh"))
(check-expect
  (editor-kh (create-editor "" "fgh") "right")
  (create-editor "f" "gh"))
(check-expect
  (editor-kh (create-editor "fgh" "") "right")
  (create-editor "fgh" ""))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor 1String -> Editor
; insert the 1String k between pre and post

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left, 
; if possible 
(define (editor-lft ed)
  (cond
    [(equal? '() (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed)) (cons (first (editor-pre ed)) (editor-post ed)))]))
 
; Editor -> Editor
; moves the cursor position one 1String right, 
; if possible 
(define (editor-rgt ed)
  (cond
    [(equal? '() (editor-post ed)) ed]
    [else (make-editor (cons (first (editor-post ed)) (editor-pre ed)) (rest (editor-post ed)))]))
 
; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible 
(define (editor-del ed)
  (cond
    [(equal? '() (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed)) (editor-post ed))]))

; [List-of 1String] N -> Editor
; the list of 1strings, ed,  represents the complete string in some editor and the number, x, represents the x-coord of a mouse click. the function produces (make-editor p s) such that:
; 1: p and s make up ed, and 2: x is larger than the image of p and smaller than the image of p extended with the first 1String on s (if any)
(define (split ed0 x)
  (local (; [List-of 1String] [List-of 1String] N -> Editor
          ; accumulator a is the 1Strings in ed0 preceding the contents of ed
          (define (split/a a ed)
            (cond
              [(zero? x) (make-editor '() ed)]
              [(>= x (image-width (editor-text ed0))) (make-editor ed0 '())]
              [else (if (<= (image-width (editor-text a)) x (image-width (editor-text (append a (list (first ed))))))
                        (make-editor a ed)
                        (split/a (cons (first ed) a) (rest ed)))])))
    (split/a '() ed0)))

; editor -> [List-of 1String]
(define (combine-editor ed)
  (append (reverse (editor-pre ed)) (editor-post ed)))
(check-expect (combine-editor (make-editor (list "l" "l" "a") (list "g" "o" "o" "d"))) (list "a" "l" "l" "g" "o" "o" "d"))

; editor int int mouse-event -> editor
(define (editor-mouse-handler input x y m-event)
  (cond
    [(string=? m-event "button-down") (split (combine-editor input) x)]
    [else input]))

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]
     [on-mouse editor-mouse-handler]))