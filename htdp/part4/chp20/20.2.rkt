;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20.2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

; EX 330
(define directory-tree.v1
  (list (list "part1" "part2" "part3") "read!" (list (list "read!") (list "hang" "draw"))))

; EX 331
; Dir.v1 -> Number
; determines how many files the Dir.v1 contains
(define (how-many direct)
  (cond
    [(empty? direct) 0]
    [(string? (first direct)) (+ 1 (how-many (rest direct)))]
    [else (+ (how-many (first direct)) (how-many (rest direct)))]))

(check-expect (how-many directory-tree.v1) 7)

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

; EX 332
(define directory-tree.v2
  (make-dir "TS"
            (list (make-dir "Text" (list "part1" "part2" "part3"))
                  "read!"
                  (make-dir "Libs" (list (make-dir "Docs" (list "read!")) (make-dir "Code" (list "hang" "draw")))))))

; EX 333
; Dir.v2 -> Number
; determines how many files the Dir.v2 contains
(define (how-many.v2 direct)
  (process-lofd (dir-content direct)))

(define (process-lofd anlofd)
  (cond
    [(empty? anlofd) 0]
    [else (cond
            [(string? (first anlofd)) (+ 1 (process-lofd (rest anlofd)))]
            [else (+ (how-many.v2 (first anlofd)) (process-lofd (rest anlofd)))])]))
  

(check-expect (how-many.v2 directory-tree.v2) 7)

; EX 334
; to add the attributes of size and permissions to directories
; add the fields in the directory structure. Size can be a number and
; permissions can be a list of pre-defined numbers like linux's permissions system

(define-struct file [name size content])

; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; EX 335
(define directory-tree.v3 (make-dir.v3 "TS"
                                       (list (make-dir.v3 "Text" '() (list (make-file "part1" 99 "")
                                                                           (make-file "part2" 52 "")
                                                                           (make-file "part3" 17 "")))
                                             (make-dir.v3 "Libs" (list (make-dir.v3 "Docs" '() (list (make-file "read!" 19 "")))
                                                                       (make-dir.v3 "Code" '() (list (make-file "hang" 8 "")
                                                                                                     (make-file "draw" 2 ""))))
                                                          '()))
                                       (list (make-file "read!" 10 ""))))

; EX 336
; count how many files for a dir.v3
(define (how-many.v3 direct)
  (+ (count-in-direc (dir.v3-dirs direct)) (count-files (dir.v3-files direct))))

(define (count-in-direc lods)
  (cond
    [(empty? lods) 0]
    [else (+ (how-many.v3 (first lods)) (count-in-direc (rest lods)))]))

(define (count-files lof)
  (cond
    [(empty? lof) 0]
    [else (+ 1 (count-files (rest lof)))]))

(check-expect (how-many.v3 directory-tree.v3) 7)

; EX 337
; simplify the data definitions for dir.v3 and the functions for ex 336 using abstraction
 
; A Dir* is one of: 
; – '()
; – [List-of Dir.v3]
 
; A File* is one of: 
; – '()
; – [List-of File.v3]

(define (how-many.v3a direct)
  (+ (foldl + 0 (map (lambda (x) (how-many.v3a x)) (dir.v3-dirs direct)))
     (length (dir.v3-files direct))))

(check-expect (how-many.v3a directory-tree.v3) 7)