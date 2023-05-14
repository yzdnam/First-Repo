#lang sicp

(define (add-assert! entries results)
  (cond ((null? entries) results)
        ((pair? (car entries)) (add-assert! (cdr entries) (cons (cons 'assert! (car entries)) results)))))

'((rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(supervisor (Fect Cy D) (Bitdiddle Ben))

(salary (Fect Cy D) 35000)
(salary (Tweakit Lem E) 25000)

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer)
(computer programmer trainee))
(can-do-job (administration secretary)
(administration big wheel)))

; EX 4.57
(rule (can-replace ?p1 ?p2)
      (and (or (and (job ?p1 ?job1)
                    (job ?p2 ?job2)
                    (same ?job1 ?job2))
               (can-do-job ?job1 ?job2))
           (not (same ?p1 ?p2))))

; EX 4.58
(rule (big-shot ?p1)
      (and (job ?p1 ?div)
           (supervisor ?p1 ?sup)
           (not (job ?sup ?div))))

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))

; EX 4.62
(assert! (rule (last-pair (?x) (?x))))
(assert! (rule (last-pair (?x . ?y) ?z)
               (last-pair ?y ?z)))

; EX 4.63 and 4.69
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?G ?S)
               (and (son ?f ?S)
                    (son ?G ?f))))
(assert! (rule (son ?M ?S)
               (and (wife ?M ?W)
                    (son ?W ?S))))

(assert! (rule (same ?x ?x)))
(assert! (rule (ends-in-gson ?x)
               (same (grandson) ?x)))
(assert! (rule (ends-in-gson (?x . ?y))
               (ends-in-gson ?y)))
(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))
(assert! (rule ((great . ?rel) ?x ?y)
               (and (son ?x ?z)
                    (?rel ?z ?y)
                    (ends-in-gson ?rel))))

(assert! (married Minnie Mickey))

(assert! (rule (married ?x ?y) (married ?y ?x)))

(assert! (rule (married ?x ?y)
               (wife ?x ?y)))
(assert! (rule (married ?y ?x)
               (married ?x ?y)))

; EX 4.68
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse (?x) (?x))))
(assert! (rule (reverse (?a . ?b) ?c)
               (and (reverse ?b ?d)
                    (append-to-form ?d (?a) ?c)
                    )))

                    
               