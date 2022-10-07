;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |8.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; Sample Problem You are working on the contact list for some new cell phone.
; The phone’s owner updates and consults this list on various occasions.
; For now, you are assigned the task of designing a function that consumes
; this list of contacts and determines whether it contains the name “Flatt.”

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [else
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

(check-expect (contains-flatt? '()) #false)

(check-expect (contains-flatt? (cons "Find" '())) #false)

(check-expect (contains-flatt? (cons "Flatt" '())) #true)

(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '())))) #true)

(check-expect (contains-flatt? (cons "no" (cons "pie" (cons "start" '())))) #false)

; EX. 134
; string list -> Boolean
; determines whether a given string occurs in a given list of strings
(define (contains? word a-list)
  (cond
    [(empty? a-list) #false]
    [else
     (cond
       [(string=? word (first a-list)) #true]
       [else (contains? word (rest a-list))])]))

(check-expect (contains? "foot" (cons "fart" (cons "faggot" (cons "fuck" '())))) #false)
(check-expect (contains? "fart" (cons "faggot" (cons "fart" '()))) #true)
(check-expect (contains? "fart" (cons "fart" (cons "faggot" '()))) #true)
(check-expect (contains? "fart" '()) #false)