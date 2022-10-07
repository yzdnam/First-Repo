;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.2pt2) (read-case-sensitive #t) (teachpacks ((lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/itunes)

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
 
; String -> LLists
; creates a list of lists representation for all tracks in 
; file-name, which must be an XML export from iTunes 
;(define (read-itunes-as-lists file-name)
;  ...)

(define ITUNES-LOCATION "itunes.xml")
 
; LLists
;(define list-tracks
;  (read-itunes-as-lists ITUNES-LOCATION))

(define (make-date a b c d e f)
  (create-date a b c d e f))

; EX 205
;examples
(define EXAMPLE-LASSOC (list
  (list "Track ID" 3992)
  (list "Name" "Monkey Man")
  (list "Artist" "Amy Winehouse")
  (list "Album Artist" "Amy Winehouse")
  (list "Composer" "Frederick Hibbert")
  (list "Album" "iTunes Festival: London 2007")
  (list "Genre" "Pop")
  (list "Kind" "Purchased AAC audio file")
  (list "Size" 6769836)
  (list "Total Time" 188813)
  (list "Disc Number" 1)
  (list "Disc Count" 1)
  (list "Track Number" 8)
  (list "Track Count" 8)
  (list "Year" 2007)
  (list "Date Modified" (create-date 2014 10 11 23 43 10))
  (list "Date Added" (create-date 2014 10 20 14 31 25))
  (list "Bit Rate" 256)
  (list "Sample Rate" 44100)
  (list "Play Count" 6)
  (list "Play Date" 3498740599)
  (list "Play Date UTC" (create-date 2014 11 13 21 23 19))
  (list "Release Date" (create-date 2007 8 13 7 0 0))
  (list "Normalization" 4904)
  (list "Artwork Count" 1)
  (list "Persistent ID" "39C9BD4120CCF2C2")
  (list "Track Type" "File")
  (list "Purchased" #true)
  (list "Location" "file://localhost/Users/matthias/Music/iTunes/iTunes%20Media/Music/Amy%20Winehouse/iTunes%20Festival_%20London%202007/08%20Monkey%20Man.m4a")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1)))
(define EXAMPLE-LLISTS (list (list
  (list "Track ID" 3990)
  (list "Name" "He Can Only Hold Her")
  (list "Artist" "Amy Winehouse")
  (list "Album Artist" "Amy Winehouse")
  (list "Composer" "Amy Winehouse, Richard Poindexter, Robert Poindexter 38 John Harrison")
  (list "Album" "iTunes Festival: London 2007")
  (list "Genre" "Pop")
  (list "Kind" "Purchased AAC audio file")
  (list "Size" 6856755)
  (list "Total Time" 191866)
  (list "Disc Number" 1)
  (list "Disc Count" 1)
  (list "Track Number" 7)
  (list "Track Count" 8)
  (list "Year" 2007)
  (list "Date Modified" (create-date 2014 10 11 23 43 14))
  (list "Date Added" (create-date 2014 10 20 14 31 25))
  (list "Bit Rate" 256)
  (list "Sample Rate" 44100)
  (list "Play Count" 6)
  (list "Play Date" 3498740411)
  (list "Play Date UTC" (create-date 2014 11 13 21 20 11))
  (list "Release Date" (create-date 2007 8 13 7 0 0))
  (list "Normalization" 5811)
  (list "Artwork Count" 1)
  (list "Persistent ID" "909150FDAC57DBBF")
  (list "Track Type" "File")
  (list "Purchased" #true)
  (list "Location" "file://localhost/Users/matthias/Music/iTunes/iTunes%20Media/Music/Amy%20Winehouse/iTunes%20Festival_%20London%202007/07%20He%20Can%20Only%20Hold%20Her.m4a")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1))
 (list
  (list "Track ID" 3992)
  (list "Name" "Monkey Man")
  (list "Artist" "Amy Winehouse")
  (list "Album Artist" "Amy Winehouse")
  (list "Composer" "Frederick Hibbert")
  (list "Album" "iTunes Festival: London 2007")
  (list "Genre" "Pop")
  (list "Kind" "Purchased AAC audio file")
  (list "Size" 6769836)
  (list "Total Time" 188813)
  (list "Disc Number" 1)
  (list "Disc Count" 1)
  (list "Track Number" 8)
  (list "Track Count" 8)
  (list "Year" 2007)
  (list "Date Modified" (create-date 2014 10 11 23 43 10))
  (list "Date Added" (create-date 2014 10 20 14 31 25))
  (list "Bit Rate" 256)
  (list "Sample Rate" 44100)
  (list "Play Count" 6)
  (list "Play Date" 3498740599)
  (list "Play Date UTC" (create-date 2014 11 13 21 23 19))
  (list "Release Date" (make-date 2007 8 13 7 0 0))
  (list "Normalization" 4904)
  (list "Artwork Count" 1)
  (list "Persistent ID" "39C9BD4120CCF2C2")
  (list "Track Type" "File")
  (list "Purchased" #true)
  (list "Location" "file://localhost/Users/matthias/Music/iTunes/iTunes%20Media/Music/Amy%20Winehouse/iTunes%20Festival_%20London%202007/08%20Monkey%20Man.m4a")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1))))

; EX 206
; string LAssoc any -> Association
; produces the first Association in the given LAssoc whose first item is equal to the given string.
; or the any-value (default) if there is no such Association
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [(equal? (first (first lassoc)) key) (first lassoc)]
    [else (find-association key (rest lassoc) default)]))

(check-expect (find-association "Year" EXAMPLE-LASSOC (first EXAMPLE-LASSOC)) (list "Year" 2007))

; EX 207
; LLists -> number
; produces total amount of play time for a given list of lists of associations
(define (total-time/list lols)
  (cond
    [(empty? lols) 0]
    [else (+ (second (find-association "Total Time" (first lols) 0)) (total-time/list (rest lols)))]))

(check-expect (total-time/list EXAMPLE-LLISTS) (+ 188813 191866))

; LLists -> LAssoc
; produces LAssoc of the "Play Date UTC" Association for all tracks in a LLists
(define (list-playdates lols)
  (cond
    [(empty? lols) (list)]
    [else (cons (find-association "Play Date" (first lols) #false) (list-playdates (rest lols)))]))

; LLists -> list-of-strings
; returns a list of the strings associated with a boolean from a list of list associations
(define (boolean-attributes lols)
  (cond
    [(empty? lols) (list)]
    [else (cond
            [(boolean-attribute? (first lols)) (create-set (append (list-boolean-attributes (first lols)) (boolean-attributes (rest lols))))]
            [else (boolean-attributes (rest lols))])]))

(check-expect (boolean-attributes EXAMPLE-LLISTS) (list "Purchased"))

; lassoc -> boolean
; returns true if a string is associated with a boolean in the given list of associations
; false otherwise
(define (boolean-attribute? lassoc)
  (cond
    [(empty? lassoc) #false]
    [(boolean? (second (first lassoc))) #true]
    [else (boolean-attribute? (rest lassoc))]))

; lassoc -> list-of-strings
; lists the strings associated with a boolean from a given list of associations
(define (list-boolean-attributes lassoc)
  (cond
    [(empty? lassoc) (list)]
    [(boolean? (second (first lassoc))) (cons (first (first lassoc)) (list-boolean-attributes (rest lassoc)))]
    [else (list-boolean-attributes (rest lassoc))]))

; list-of-strings -> list-of-strings
; returns a list with all duplicate strings eliminated from the original list (a set)
(define (create-set los)
  (cond
    [(empty? los) (list)]
    [else (cond
            [(member? (first los) (rest los)) (create-set (rest los))]
            [else (cons (first los) (create-set (rest los)))])]))

; LAssoc -> Track
; converts an LAssoc to a track structure when possible
(define (track-as-struct lassoc)
    (create-track (association-value "Name" lassoc) (association-value "Artist" lassoc) (association-value "Album" lassoc)
                      (association-value "Total Time" lassoc) (association-value "Track Number" lassoc) (association-value "Date Added" lassoc)
                      (association-value "Play Count" lassoc) (association-value "Play Date UTC" lassoc)))

(check-expect (track-as-struct EXAMPLE-LASSOC) (create-track "Monkey Man" "Amy Winehouse" "iTunes Festival: London 2007" 188813 8
                                                           (make-date 2014 10 20 14 31 25) 6 (make-date 2014 11 13 21 23 19)))

; lassoc string -> BSDN
; returns the boolean, string, date, or number associated with the given string in the given LAssoc
; error otherwise
(define (association-value str lassoc)
  (second (find-association str lassoc "error: track does not have all required information for conversion")))
