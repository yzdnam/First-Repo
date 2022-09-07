;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

;(define Hlp (create-dir "/home/acerlaptop/documents/2_school"))
(define Hdt (create-dir "/home/admin/documents/2_school/cs/lab"))

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

(define EXAMPLE-TREE (make-dir "TS"
                                       (list (make-dir "Text" '() (list (make-file "part1" 99 "")
                                                                        (make-file "part2" 52 "")
                                                                        (make-file "part3" 17 "")))
                                             (make-dir "Libs" (list (make-dir "Docs" '() (list (make-file "read!" 19 "")))
                                                                    (make-dir "Code" '() (list (make-file "hang" 8 "")
                                                                                               (make-file "draw" 2 ""))))
                                                          '()))
                                       (list (make-file "read!" 10 ""))))

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

(check-expect (find? EXAMPLE-TREE "read!") #true)
(check-expect (find? EXAMPLE-TREE "fart") #false)

; EX 340
; Dir -> [List-of String]
; lists the names of all files and directories in a given Dir
(define (ls direct)
  (append (foldl cons '() (map dir-name (dir-dirs direct))) (foldl cons '() (map file-name (dir-files direct)))))

(define (ls-dirs lod)
  (cond
    ([empty? lod] '())
    (else (cons (dir-name (first lod)) (ls-dirs (rest lod))))))

(define (ls-files lof)
  (foldl cons '() (map file-name lof)))

; EX 341
; Dir -> Number
; computes the size of a directory tree assuming a directory has a size of 1
(define (du direct)
  (+ 1 (dir*-size (dir-dirs direct)) (file*-size (dir-files direct))))

(define (dir*-size lod)
  (cond
    [(empty? lod) 0]
    [else (+ (du (first lod)) (dir*-size (rest lod)))]))

(define (file*-size lof)
  (cond
    [(empty? lof) 0]
    [else (+ (file-size (first lof)) (file*-size (rest lof)))]))

(check-expect (du EXAMPLE-TREE) 212)

; A Path is [List-of String].
; interpretation directions into a directory tree

; EX 342
; Dir string -> [List-of String]
; if (find? dir filename) is #true, find produces a path to a file with name filename
; otherwise it produces #false
(define (find direct fname)
  (cond
    [(find? direct fname) (cons (dir-name direct) (if (find-in-file* (dir-files direct) fname) (path-to-file (dir-files direct) fname)
                                                   (search-dir* (dir-dirs direct) fname)))]
    [else #false]))

; [List-of Dir] string -> [List-of String]
(define (search-dir* lod fname)
  (cond
    [(empty? lod) '()]
    [else (if (find? (first lod) fname) (find (first lod) fname)
              (search-dir* (rest lod) fname))]))

(define (path-to-file lof fname)
  (cond
    [(empty? lof) '()]
    [else (if (equal? (file-name (first lof)) fname) (list fname)
              (path-to-file (rest lof) fname))]))

; Dir string -> [List-of [List-of String]]
; produces the list of all paths that lead to f in d
(define (find-all direct fname)
  (cond
    [(and (not (empty? (path-to-file (dir-files direct) fname)))
          (not (empty? (find-all-dirs (dir-dirs direct) fname)))) (cons (cons (dir-name direct) (path-to-file (dir-files direct) fname))
                                                                      (map (lambda (x) (cons (dir-name direct) x)) (find-all-dirs (dir-dirs direct) fname)))]
    
    [(and (not (empty? (path-to-file (dir-files direct) fname)))
          (empty? (find-all-dirs (dir-dirs direct) fname))) (cons (dir-name direct) (path-to-file (dir-files direct) fname))]
    
    [(and (empty? (path-to-file (dir-files direct) fname))
          (not (empty? (find-all-dirs (dir-dirs direct) fname)))) (map (lambda (x) (cons (dir-name direct) x)) (find-all-dirs (dir-dirs direct) fname))]
    
    [else #false]))

(define (find-all-dirs lod fname)
  (cond
    [(empty? lod) '()]
    [(find? (first lod) fname) (cons (find (first lod) fname) (find-all-dirs (rest lod) fname))]
    [else (find-all-dirs (rest lod) fname)]))

(check-expect (find EXAMPLE-TREE "read!") (list "TS" "read!"))
(check-expect (find EXAMPLE-TREE "hang") (list "TS" "Libs" "Code" "hang"))
(check-expect (find-all EXAMPLE-TREE "read!") (list (list "TS" "read!") (list "TS" "Libs" "Docs" "read!")))

; EX 343
; Dir -> [List-of [List-of String]]
; lists the paths to all files contained in a given Dir
(define (ls-r direct)
  (cons (show-all-paths (dir-dirs direct))
        (start-paths direct (dir-files direct))))

(define (show-all-paths lod)
  (cond
    [(empty? lod) '()]
    [else (append (cons (dir-name (first lod)) (ls-r (first lod))) (show-all-paths (rest lod)))]))

(define (start-paths direct lof)
  (cond
    [(empty? lof) '()]
    [else (cons (file-name (first lof)) (start-paths direct (rest lof)))]))
  
(ls-r Hdt)
