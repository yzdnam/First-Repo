;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chp16ex276) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; An LTracks is one of:
; – '()
; – (cons Track LTracks)
 
;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played
 
;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
;(define (create-track name artist album time
;                      track# added play# played)
;  ...)

; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs 
; otherwise it produces #false
;(define (create-date y mo day h m s)
;  ...)
 
; String -> LTracks
; creates a list-of-tracks representation from the
; text in file-name (an XML export from iTunes)
;(define (read-itunes-as-tracks file-name)
;  ...)

(define ITUNES-LOCATION "itunes.xml")
 
; LTracks, uncomment when wanting to compute information about iTunes collection
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; EX 199
; examples of
; dates
(define EXAMPLE-DATE (create-date 12 12 12 12 12 12))
; tracks
(define EXAMPLE-TRACK (create-track "the real slim shady" "eminem" "slim shady lp" 1000 12 (create-date 12 12 12 12 12 12) 10 (create-date 10 12 12 10 12 12)))
; LTracks
(define EXAMPLE-LTRACKS (list 
(create-track "the real slim shady" "eminem" "slim shady lp" 1000 12 (create-date 12 12 12 12 12 12) 10 (create-date 10 12 12 10 12 12))
(create-track "my band" "eminem" "d12 album" 2000 12 (create-date 12 12 12 12 12 12) 27 (create-date 10 12 12 10 12 12))))

; list-of-strings -> list-of-strings
; returns a list with all duplicate strings eliminated from the original list (a set)
(define (create-set los)
  (local (
          (define (cons-or-not item base)
            (cond
              [(member? item base) base]
              [else (cons item base)])))
    (foldr cons-or-not '() los)))

(check-expect (create-set (select-album-titles/unique (cons EXAMPLE-TRACK EXAMPLE-LTRACKS))) (list "slim shady lp" "d12 album"))

; EX 200
; LTracks -> Number
; consumes an LTracks and returns the total playing time
(define (total-time lots)
  (local (
          (define (add-up-time t1 base)
            (+ (track-time t1) base)))
    (foldr add-up-time 0 lots)))

(check-expect (total-time EXAMPLE-LTRACKS) 3000)

; LTracks -> list-of-strings
; consumes an LTracks and returns a list of unique album titles
(define (select-album-titles/unique lots)
  (create-set (map track-album lots)))

; EX 202
; string LTracks -> LTracks
; consumes the title of an album and an LTracks
; extracts from the latter the list of tracks that belong to the given album
(define (select-album alb lots)
  (local (
          (define (in-album? track)
            (equal? (track-album track) alb)))
    (filter in-album? lots)))

(check-expect (select-album "d12 album" EXAMPLE-LTRACKS) 
(list (create-track "my band" "eminem" "d12 album" 2000 12 (create-date 12 12 12 12 12 12) 27 (create-date 10 12 12 10 12 12))))

; LTracks -> list-of-LTracks
; produces a list of LTracks, one per album
(define (select-albums lots)
  (create-album-lists (select-album-titles/unique lots) lots))

; list-of-strings LTracks -> list-of-LTracks
(define (create-album-lists albs lots)
  (local (
          (define (create-entry album)
            (cons album (select-album album lots))))
    (map create-entry albs)))

; string date LTracks -> LTracks
; consumes an album, a date, and an LTracks.
; extracts from the LTracks the list of tracks belonging to the album that have been played
; after the given date
(define (select-album-date alb dte lots)
  (local (
          ; LTracks date -> LTracks
          ; returns a list of tracks from the given list that have been played after the given date
          (define (played-after? lots)
            (local (
                    (define (played-after-filter track)
                      (after? (track-played track) dte)))
              (filter played-after-filter lots))))
    
  (played-after? (select-album alb lots))))

(check-expect (select-album-date "d12 album" (create-date 10 12 12 10 12 10) EXAMPLE-LTRACKS)
              (list (create-track "my band" "eminem" "d12 album" 2000 12 (create-date 12 12 12 12 12 12) 27 (create-date 10 12 12 10 12 12))))

; abstraction helper for after?
(define (after?-x-y cur-level lower-level date1 date2)
  (cond
    [(> (cur-level date1) (cur-level date2)) #true]
    [(equal? (cur-level date1) (cur-level date2)) (lower-level date1 date2)]
    [else #false]))

; date date -> boolean
; compares two dates, if the first date is after the second date, returns #true
; else #false
(define (after? date1 date2)
  (local (
          (define (after?-month date1 date2)
            (local (
                    (define (after?-day date1 date2)
                      (local (
                              (define (after?-hour date1 date2)
                                (local (
                                        (define (after?-minute date1 date2)
                                          (local (
                                                  (define (after?-second date1 date2)
                                                    (cond
                                                      [(> (date-second date1) (date-second date2)) #true]
                                                      [(equal? (date-second date1) (date-second date2))
                                                       (error "songs in the list have been played at exact same time")]
                                                      [else #false])))
                                          (after?-x-y date-minute after?-second date1 date2))))
                                (after?-x-y date-hour after?-minute date1 date2))))
                      (after?-x-y date-day after?-hour date1 date2))))
           (after?-x-y date-month after?-day date1 date2))))
  (after?-x-y date-year after?-month date1 date2)))


; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))