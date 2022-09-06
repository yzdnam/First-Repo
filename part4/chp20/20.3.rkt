;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

(define H (create-dir "/home/acerlaptop/documents/2_school"))

; A File.v3 is a structure: 
;   (make-file String N String)

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)

; A Dir* is one of: 
; – '()
; – [List-of Dir.v3]
 
; A File* is one of: 
; – '()
; – [List-of File.v3]

; EX 338
(define (how-many direct)
  (+ (foldl + 0 (map (lambda (x) (how-many x)) (dir-dirs direct)))
     (length (dir-files direct))))

; EX 339
; Dir.v3 string -> Boolean
; determines whether the given string is the name of a file in the given directory tree or not
(define (find? direct fname)
  
