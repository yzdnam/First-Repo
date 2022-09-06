;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

;(define Hlp (create-dir "/home/acerlaptop/documents/2_school"))
(define Hdt (create-dir "/home/admin/documents/2_school/cs/htc/first-repo"))

; A File is a structure: 
;   (make-file String N String)

; A Dir is a structure: 
;   (make-dir.v3 String Dir* File*)

; A Dir* is one of: 
; – '()
; – [List-of Dir]
 
; A File* is one of: 
; – '()
; – [List-of File]

;(define-struct file [name size content])

;(define-struct dir [name dirs files])

; EX 338
(define (how-many direct)
  (+ (foldl + 0 (map (lambda (x) (how-many x)) (dir-dirs direct)))
     (length (dir-files direct))))

; EX 339
; Dir string -> Boolean
; determines whether the given string is the name of a file in the given directory tree or not
(define (find? direct fname)
  (or (find-in-dir* (dir-dirs direct) fname)
      (find-in-file* (dir-files direct) fname))) 

(define (find-in-dir* lod fname)
  (cond
    [(empty? lod) #false]
    [else (or (find? (first lod) fname)
              (find-in-dir* (rest lod) fname))]))

(define (find-in-file* lof fname)
  (cond
    [(empty? lof) #false]
    [else (if (equal? (file-name (first lof)) fname)
              #true
              (find-in-file* (rest lof) fname))]))

(check-expect (find? Hdt "20.2.rkt") #true)
(check-expect (find? Hdt "fart") #false)

; EX 340
; Dir -> [List-of String]
; lists the names of all files and directories in a given Dir
(define (ls direct)
  (append (ls-dirs (dir-dirs direct)) (ls-files (dir-files direct))))

(define (ls-dirs lod)
  (cond
    ([empty? lod] '())
    (else (cons (dir-name (first lod)) (ls-dirs (rest lod))))))

(define (ls-files lof)
  (foldl cons '() (map file-name lof)))