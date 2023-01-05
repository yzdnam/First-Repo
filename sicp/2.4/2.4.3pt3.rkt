#lang sicp

; EX 2.74
; a corporation's divisions all use different data structures for their files
; show how to implement a strategy, using data-directed programming, to integrate the corporation's files across its divisions

; a. Implement for headquarters a get-record procedure that retrieves a specified employee’s record from a specified personnel
; file. The procedure should be applicable to any division’s file.
(define (get-record division employee-name)
  ((get 'get-record division) employee-name))

; Explain how the individual divisions’ files should be structured. In particular, what type information must be supplied?

; a unique identifier for each division should be used as a type-tag for each set of employee records originating from a division
; signifying the division under which the record is kept and, in turn, how the data in that record is structured.
; example:
(define div1-file `(div1 (employee1 (address ,"123 circle street") (salary ,11.50))
                         (employee2 (address ,"124 circle street") (salary ,12.50))))

; b. Implement for headquarters a get-salary procedure that returns the salary information from a given employee’s record from
; any division’s personnel file. How should the record be structured in order to make this operation work?
(define (get-salary employee-record)
  ((get 'get-salary (get-division employee-record)) employee-record))
; to make this operation work, a type-tag signifying the division in which the given record is found needs to be built into the
; structure of the record. this allows the get-salary procedure to retrieve the proper procedure for dealing with the type of
; data structure identified (using the division type-tag) from the operation-type table

; c. Implement for headquarters a find-employee-record procedure. This should search all the divisions’ files for the record of
; a given employee and return the record. Assume that this procedure takes as arguments an employee’s name and a list of all
; the divisions’ files.
(define (find-employee-record employee-name list-of-div-files)
  (cond
    ((null? list-of-div-files) (error "Employee record not found"))
    ((in-division? employee-name (car list-of-div-files)) (get-record (car list-of-div-files) employee-name))
    (else (find-employee-record employee-name (cdr list-of-div-files)))))

; d. when the corporation takes over a new company, what changes must be made to incorporate the new personnel information into
; the central system

; a package of utilities compatible with the new personnel information must be installed into the central system along with the
; put operations which will interface the utilities with the rest of the system. the personnel file, along with each record
; contained in it, must also be affixed with a tag identifying its location/type so that the central utilities can call the
; appropriate division-specific utilities when called on the new file and records.

; EX 2.75
; implement make-from-mag-ang in message-passing style.
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else (error "Uknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; EX 2.76
; for each of the 3 strategies for implementing generic operations, explicit dispatch, data-directed style, and message-passing style, describe the changes that must
; be made to a system in order to add new types or new operations.

; explicit dispatch:
; to add new types, each operation in the system would need to be modified with a new condition handling the new type of data
; to add new operations, define a new operation with a conditional that handles each type of data in the system

; data-directed style:
; to add new types, install a package of utilities that handle the new type of data and interface the package with the rest of the system using a put procedure.
; operations in the system will interface with the new type using the get procedure
; to add new operations, the operation would need to be defined within each type package.

; message-passing style:
; to add new types, define a new constructor procedure consisting of a conditional that handles each operation in the system for that data-type
; to add new operations, each type in the system would need to be modified with a new condition that dispatches the new operation

; in a system where new types must often be added but new operations are not, message-passing is most appropriate.
; in a system where new operations must often be added but new types are not, explicit dispatch would be most appropriate