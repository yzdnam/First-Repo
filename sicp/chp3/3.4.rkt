#lang sicp

; Section 3.4 Concurrency: Time is of the Essence

; 3.4.1 The Nature of Time in Concurrent Systems

; EX 3.38
; Peter, Paul and Mary share a joint bank account that initially contains $100. concurrently, peter deposits $10, Paul withdraws $20, and Mary withdraws half the money in the account,
; by executing the following commands:
(define balance 100)

; a. list all the different possible values for balance after these three transactions have been completed, assuming that the banking system forces the tree processes to run
; sequentially in some order:
(define (test-account)
  (let ((balance 100))
    (define (Peter) (set! balance (+ balance 10)))
    (define (Paul) (set! balance (- balance 20)))
    (define (Mary) (set! balance (- balance (/ balance 2))))
    (define (me request)
      (cond
        ((eq? request 'pe) (Peter)
                           )
        ((eq? request 'pa) (Paul)
                           )
        ((eq? request 'ma) (Mary)
                           )
        ((eq? request 'bal) balance)))
    me))
(define f1 (test-account))
(f1 'pe)
(f1 'pa)
(f1 'ma)
(f1 'bal)
; Peter
; Paul
; Mary

(define f2 (test-account))
(f2 'pe)
(f2 'ma)
(f2 'pa)
(f2 'bal)
; Peter
; Mary
; Paul

(define f3 (test-account))
(f3 'ma)
(f3 'pe)
(f3 'pa)
(f3 'bal)
; Mary
; Peter
; Paul

(define f4 (test-account))
(f4 'ma)
(f4 'pa)
(f4 'pe)
(f4 'bal)
; Mary
; Paul
; Peter

(define f5 (test-account))
(f5 'pa)
(f5 'pe)
(f5 'ma)
(f5 'bal)
; Paul
; Peter
; Mary

(define f6 (test-account))
(f6 'pa)
(f6 'ma)
(f6 'pe)
(f6 'bal)
; Paul
; Mary
; Peter

; 3.4.2 Mechanisms for Controlling Concurrency

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))
; test-and-set! must be performed atomically. if not, then the mutex can fail in a way similar to the bank-account failure in Figure 3.29

(define (clear! cell) (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; EX 3.39
; which of the five possibilities in the parallel execution shown in the text remain if we serialize execution as follows:
(define x 10)
(define s (make-serializer))
;(parallel-execute
; (lambda () (set! x ((s (lambda () (* x x))))))
; (s (lambda () (set! x (+ x 1)))))

; 101: P1 sets x to 100 and then P2 increments x to 101.
; 121: P2 increments x to 11 and then P1 sets x to x * x.
; 11: P2 accesses x, then P1 sets x to 100, then P2 sets x.
; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

; EX 3.40
; Give all possible values of x that can result from executing:
;(define x3.40 10)
;(parallel-execute (lambda () (set! x (* x x)))
;                  (lambda () (set! x (* x x x))))
; during any step of either procedure included in the parallel-execute, the other procedure can interleave itself creatinga a set of possibile outputs too large for any practical use
; if we were to serialize the procedures, only two possibilities for the parallel execution would exist, 100^3 or 1000^2

; EX 3.41
; the local procedure to check the state of a bank account's balance does not need to be serialized because it can be interleaved into the execution of a withdraw or deposit
; operation and the end-state of the bank account will be identical to one where the balance check was not conducted during the execution of the withdraw or deposit

; EX 3.42
; SICP solution page says ben bitdiddle's solution is essentially the same as the original but without further knowledge of how make-serial and parallel-execute work, it's hard to
; tell whether a serialized function can be interleaved with itself. if so, then bitdiddle's solution will not return the same set of outputs as the original function

; original make-account for 3.4
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; EX 3.43
; suppose the balances of 3 accounts start as $10, $20, $30 and that multiple processes run, exchanging the balances in the accounts. argue that if the processes are run
; sequentially, after any number of concurrent exchanges, the account balances should be $10, $20, $30 in some order:

; the given circumstance will stand if the exchange processes are run sequentially because each call to exchange will end with the same set of account balances.

; draw a timing diagram to show how this condition can be violated if the exchanges are implemented using the first version of the account-exchange program
; hand-drawn

; now argue that even with this exchange program, the sum of the balances in the accounts will be preserved:

; the sum of the balances in the accounts will be preserved using the original exchange program because each withdraw or deposit operation on a single account will run sequentially.
; this ensures that the computations of new values for balances always uses the last completed operation's assignment which preserves the sum of all the account balances by
; preventing erroneous increases or decreases in balances through the interleaving of access and computations steps by procedures running concurrently on the same account

; EX 3.44
; consider the following transfer procedure:
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
; will this procedure preserve the amount of money within the banking system even if there are multiple people concurrently transferring money among multiple accounts?
; assume the account mechanism serializes deposit and withdrawal transactions like the make-account found in the text and above

; it will. the essential difference between the transfer problem and the exchange problem is that the exchange problem requires the accounts to maintain their given set of balances
; the transfer problem does not require this. two accounts can transfer money to a third account and the order in which the withdrawals and deposits occur is irrelevant. the
; sum of money contained within the three accounts is what is relevant and that is preserved with the serialized procedures within the individual accounts

; EX 3.45
; consider the following account mechanism where the account's serializer is exported IN ADDITION TO using it to serialize withdrawals and deposits:
(define (make-account-and-serializer3.45 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

; explain what happens when serialized-exchange is called with the above account mechanism
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
; the procedure would call an error because it would be calling its serializer to serialize itself.

; EX 3.46
; draw a timing diagram demonstrating how mutex implementation can fail by allowing two processes to acquire the mutex at the same time if test-and-set! is implemented as an ordinary
; procedure
; see notes for diagram


; EX 3.47
; a semaphore is like a mutex that supports up to n processes concurrently. give implementations of semaphores
; a. in terms of mutexes
(define (make-mutex-semaphore n)
    (let ((mutex-table (list '*table*)))
      (define (create-mutex-table n)
        (if (> n 0) (begin
                      (set-cdr! mutex-table
                                (cons (cons 'open (make-mutex))
                                      (cdr mutex-table)))
                      (create-mutex-table (dec n)))))
      (define (dispatch m)
        (cond ((eq? m 'acquire) (let ((open-mutex (find-open-mutex (cdr mutex-table))))
                                  (set-car! open-mutex 'closed)
                                  ((cdr open-mutex) 'acquire)
                                  ;mutex-table
                                  ))
              ((eq? m 'release) (let ((closed-mutex (find-closed-mutex (cdr mutex-table))))
                                  (set-car! closed-mutex 'open)
                                  ((cdr closed-mutex) 'release)
                                  ;mutex-table
                                  ))
              ))
      (create-mutex-table n)
      dispatch
      ))
      
(define (find-open-mutex tbl0)
  (define (find-open-mutex/i tbl)
    (cond ((null? tbl) (find-open-mutex/i tbl0))
          ((eq? 'open (caar tbl)) (car tbl))
          (else (find-open-mutex/i (cdr tbl)))))
  (find-open-mutex/i tbl0))

(define (find-closed-mutex tbl0)
  (define (find-closed-mutex/i tbl)
    (cond ((null? tbl) (error "no closed mutexes found"))
          ((eq? 'closed (caar tbl)) (car tbl))
          (else (find-closed-mutex/i (cdr tbl)))))
  (find-closed-mutex/i tbl0))

;(define f (make-mutex-semaphore 3))
;(f 'acquire)
;(f 'release)

; b. in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((cells (list '*cells)))
    (define (create-cells n)
      (if (> n 0) (begin
                    (set-cdr! cells
                              (cons (list false) (cdr cells)))
                    (create-cells (dec n)))))
    (define (test-cells cells0)
      (define (test-cells/i cells)
        (cond ((null? cells) (test-cells/i cells0))
              (else (if (test-and-set! (car cells))
                        (test-cells/i (cdr cells))))))
      (test-cells/i cells0))
    (define (release-a-cell cells)
      (if (null? cells)
          (error "all cells are released")
          (if (caar cells)
              (set-car! (car cells) false)
              (release-a-cell (cdr cells)))))
    (define (dispatch m)
      (cond ((eq? m 'acquire) (test-cells (cdr cells))
                              cells
                              )
            ((eq? m 'release) (release-a-cell (cdr cells))
                              cells
                              )))
    (create-cells n)
    dispatch))

(make-semaphore 2)
(define v (make-semaphore 2))
(v 'acquire)
(v 'acquire)
(v 'release)
(v 'release)

; EX 3.48
; explain why numbering accounts and forcing each process to acquire the smaller-numbered account first avoids deadlock in the exchange problem:

; because the processes accessing the accounts will all start at the same point and traverse the accounts in a sequential manner. their progress will be similar to a queue instead of
; the scenario described in the text where one process, p1, locks another process, p2, out of account, a1, while p2 locks p1 out of another account, a2.

; rewrite make-account and  serialized-exchange to incorporate this idea
(define current-acc-# (list 0))
(define (make-account-and-serializer3.48 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (account-id (car current-acc-#)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) account-id)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    (set-car! current-acc-# (inc (car current-acc-#)))
    dispatch))

(define test-acc (make-account-and-serializer3.48 100))
(test-acc 'id)
(define test-acc1 (make-account-and-serializer3.48 100))
(test-acc1 'id)

(define (serialized-exchange3.48 account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account2
         account1)
    )))

; EX 3.49
; describe a scenario where the deadlock-avoidance mechanism above does not work. consider a situation where a process must get access to some shared resources before it can know
; which additional shared resources it will require

; the hypothetical process in this scenario takes as an input an account, a1, a set of accounts that the first account is a member of, AS, and an amount, n. the process will traverse
; AS searching for an account with a large enough balance to exchange n with a1. suppose this process is executed concurrently by two users with the same set of accounts and an n
; large enough to where the only accounts in AS capable of executing the exchange are the single accounts given by the users, a1 for the first user, and a2 for the other user. this
; scenario will result in a deadlock when the first process attempts to access a2 and the second process attempts to access a1 simultaneously