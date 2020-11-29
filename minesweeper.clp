;;; #######################
;;; DEFTEMPLATES & DEFFACTS
;;; #######################

(deftemplate cell
    (slot id) ;; cell id
    (slot row)
    (slot column)
)

(deftemplate cell-value
    (slot id) ;; cell id
    (slot value)
)

(deftemplate check-cell
    (slot row)
    (slot column)
)

(deftemplate move
    (slot id) ;; cell id
)

(deftemplate move-pool
    (multislot id) ;; cell id
)

(deftemplate last-move
    (slot id) ;; cell id
)

(deftemplate mark-mine
    (slot id) ;;; cell id
)

(deftemplate win-counter
    (slot count)
)

(deftemplate mark-safe
    (slot id) ;;; cell id
)

(deftemplate sentence-cell
    (multislot id) ;; cell ids
)

(deftemplate last-sentence
    (slot idx) ;; sentence id
)

(deftemplate sentence-counter
    (slot idx) ;; sentence id
)

(deftemplate sentence
    (multislot id) ;; cell ids
    (slot count) ;; mine-count  
)

(deftemplate mine
    (slot row)
    (slot column)
)

;;; ###########
;;; SETUP RULES
;;; ###########
;;; **********
;;; initialize
;;; **********

(defrule initialize

    =>

    (assert (phase read-file))

    (printout t "Input File Name" crlf)
    (assert (filename (read)))
)

(defrule read-file

    ?f <- (phase read-file)
    (filename ?fn)

    =>

    (retract ?f)
    (assert (phase generate-board))

    (open ?fn data "r")
    
    (bind ?d (readline data))
    (assert (size (string-to-field ?d)))

    (bind ?d (readline data))
    (assert (mine-count (string-to-field ?d)))
    
    (bind ?d (readline data))
    (while (neq ?d EOF)
        (assert (mine (row (nth$ 1 (explode$ ?d))) (column (nth$ 2 (explode$ ?d)))))
        (bind ?d (readline data)))

    (close data)
)

(defrule generate-board

    ?f <- (phase generate-board)
    (size ?s)
    (mine-count ?mc)

    =>

    (retract ?f)
    (assert (phase move-start))

    (assert (last-sentence (idx 0)))
    (assert (sentence-counter (idx 1)))
    (assert (last-move (id -1)))
    (bind $?sc nil)
    (bind $?sc (delete-member$ (create$ $?sc) $?sc))

    (bind ?id 1)
    (loop-for-count (?cnt 0 (- ?s 1)) do
        (loop-for-count (?cnt2 0 (- ?s 1)) do
            (assert (cell (id ?id) (row ?cnt) (column ?cnt2)))
            (assert (cell-value (id ?id) (value -1)))
            (bind $?sc (insert$ (create$ $?sc) 1 ?id))
            (bind ?id (+ ?id 1))
        )
    )
    (assert (sentence (id ?sc) (count ?mc)))
    (assert (move-pool (id ?sc)))
)

(defrule move-start

    ?f <- (phase move-start)
    ?l <- (last-move (id $?))
    ?mp <- (move-pool (id $?mpid))
    (size ?s)
    
    =>

    (retract ?f)
    (assert (phase probe-cell))
    
    (bind ?id 1)

    (modify ?mp (id (delete-member$ (create$ $?mpid) ?id)))

    (assert (move (id ?id)))
    (modify ?l (id ?id))
)

(defrule probe-cell-mine

    (declare (salience 10))

    ?f <- (phase probe-cell)
    (last-move (id ?id))
    (cell (id ?id) (row ?r) (column ?c))
    (mine (row ?r) (column ?c))

    =>

    (retract ?f)
    (assert (phase print))
    (assert (phase lose))
    (assert (print 1))
    (assert (detonate ?id))
)

(defrule probe-cell-value

    ?f <- (phase probe-cell)
    (last-move (id ?id))
    (cell (id ?id) (row ?r) (column ?c))
    ?v <- (cell-value (id ?id))

    =>

    (retract ?f)
    (assert (phase check-value))

    (modify ?v (value 0))
    (assert (sentence-cell))
    
    (loop-for-count (?cnt -1 1) do
        (loop-for-count (?cnt2 -1 1) do
            (if (not (and (= ?cnt 0) (= ?cnt2 0)))
                then
                (assert (check-cell (row (+ ?r ?cnt)) (column (+ ?c ?cnt2))))
            )
        )   
    )
)

(defrule check-filter

    (declare (salience 10))

    (phase check-value)
    (size ?s)
    (or (or (check-cell (row ?r&:(< ?r 0)) (column ?c)) (check-cell (row ?r&:(>= ?r ?s)) (column ?c))) (or (check-cell (row ?r) (column ?c&:(< ?c 0))) (check-cell (row ?r) (column ?c&:(>= ?c ?s)))))
    ?f <- (check-cell (row ?r) (column ?c))

    =>

    (retract ?f)

)

(defrule check-value-mine

    (declare (salience 5))

    (phase check-value)
    ?cc <- (check-cell (row ?r) (column ?c))
    ?sc <- (sentence-cell (id $?sid))
    (mine (row ?r) (column ?c))
    (cell (id ?id) (row ?r) (column ?c))

    (last-move (id ?lid))
    ?cv <- (cell-value (id ?lid) (value ?v))

    =>

    (retract ?cc)
    (modify ?sc (id (insert$ (create$ $?sid) 1 ?id)))
    (modify ?cv (value (+ ?v 1)))

)

(defrule check-value

    (phase check-value)
    ?cc <- (check-cell (row ?r) (column ?c))
    ?sc <- (sentence-cell (id $?sid))
    (cell (id ?id) (row ?r) (column ?c))

    =>

    (retract ?cc)
    (modify ?sc (id (insert$ (create$ $?sid) 1 ?id)))
)

(defrule end-check

    (declare (salience -5))

    ?f <- (phase check-value)
    (not (check-cell (row $?) (column $?)))
    ?sc <- (sentence-cell (id $?sid))
    ?ls <- (last-sentence(idx ?idx))
    ?scx <- (sentence-counter(idx ?cidx))

    (last-move (id ?lid))
    (cell-value (id ?lid) (value ?v))

    =>

    (retract ?f)
    (retract ?sc)
    (assert (phase update-knowledge))
    (modify ?ls (idx (+ ?cidx 1)))
    (modify ?scx (idx (+ ?cidx 2)))

    (assert (sentence (id ?lid) (count 0)))
    (assert (sentence (id $?sid) (count ?v)))
)

(defrule sentence-subtraction

    (declare (salience 20))

    (phase update-knowledge)
    ?s1 <- (sentence (id $?sid&:(> (length$ (create$ $?sid)) 1)) (count ?v))
    ?s2 <- (sentence (id $?sid2&:(and (neq (create$ $?sid2) (create$ $?sid)) (subsetp (create$ $?sid2) (create$ $?sid)))) (count ?v2))

    =>

    (if (subsetp (create$ $?sid2) (create$ $?sid))
        then
        (bind $?sid3 (create$ $?sid))
        (loop-for-count (?cnt 1 (length$ $?sid2)) do
            (bind $?sid3 (delete-member$ $?sid3 (nth$ ?cnt $?sid2)))
        )
        (modify ?s1 (id $?sid3) (count (- ?v ?v2)))
    )
)


(defrule sentence-atom-safe

    (declare (salience 10))

    (phase update-knowledge)
    ?s <- (sentence (id $?id&:(> (length$ (create$ $?id)) 1)) (count ?cnt&:(= ?cnt 0)))

    =>

    (retract ?s)

    (loop-for-count (?cnt 1 (length$ $?id))
        (bind ?idn (nth$ ?cnt (create$ $?id)))
        (assert (sentence (id ?idn) (count 0)))
        (assert (mark-safe (id ?idn)))
    )
)

(defrule sentence-atom-mine

    (declare (salience 10))

    (phase update-knowledge)
    ?s <- (sentence (id $?id&:(> (length$ (create$ $?id)) 1)) (count ?cnt&:(= ?cnt (length$ (create$ $?id)))))

    =>

    (retract ?s)

    (loop-for-count (?cnt 1 (length$ $?id))
        (bind ?idn (nth$ ?cnt (create$ $?id)))
        (assert (sentence (id ?idn) (count 1)))
    )
)

(defrule atom-safe

    (declare (salience 5))
    (phase update-knowledge)
    ?s <- (sentence (id $?id&:(= (length$ (create$ $?id)) 1)) (count ?cnt&:(= ?cnt 0)))
    (not (mark-safe (id ?idm&:(= ?idm (nth$ 1 $?id)))))

    =>

    (assert (mark-safe (id (nth$ 1 $?id))))
)

(defrule atom-mine

    (declare (salience 5))

    (phase update-knowledge)
    ?s <- (sentence (id $?id&:(= (length$ (create$ $?id)) 1)) (count ?cnt&:(= ?cnt 1)))
    (not (mark-mine (id ?idm&:(= ?idm (nth$ 1 $?id)))))
    ?mp <- (move-pool (id $?mpid))

    =>
    
    (bind ?idn (nth$ 1 $?id))
    (assert (mark-mine (id ?idn)))
    (modify ?mp (id (delete-member$ (create$ $?mpid) ?idn)))
)

(defrule end-sentence-subtraction
    
    ?f <- (phase update-knowledge)

    =>

    (retract ?f)
    (assert (phase trans-move))
)

(defrule move-safe

    (declare (salience 10))

    ?f <- (phase move)
    (mark-safe (id ?id))
    (not (move (id ?id)))
    ?l <- (last-move (id $?))
    ?mp <- (move-pool (id $?mpid))

    =>

    (retract ?f)
    (assert (phase probe-cell))

    (modify ?mp (id (delete-member$ (create$ $?mpid) ?id)))

    (assert (move (id ?id)))
    (modify ?l (id ?id))
)

(defrule move-prob

    (declare (salience 5))

    ?f <- (phase move)
    ?l <- (last-move (id $?))
    (size ?s)
    (not (move (id ?id)))
    ?mp <- (move-pool (id $?mpid))

    =>

    (retract ?f)
    (assert (phase probe-cell))


    (bind ?n (+ (mod (random) (length$ (create$ $?mpid))) 1))
    (bind ?id (nth$ ?n (create$ $?mpid)))

    (modify ?mp (id (delete-member$ (create$ $?mpid) ?id)))

    (assert (move (id ?id)))
    (modify ?l (id ?id))
)

(defrule trans-move

    ?f <- (phase trans-move)

    =>

    (retract ?f)
    (assert (phase move))
)

(defrule check-con

    (declare (salience 20))

    ?f <- (phase trans-move)
    (move-pool (id $?id&:(= (length$ (create$ $?id)) 0)))

    =>

    (retract ?f)
    (assert (phase check-con))

    (assert (mine-correct 0))
)

(defrule check-won

    (declare (salience 30))

    (mine-count ?mcc)
    ?f <- (phase check-con)
    ?mc <- (mine-correct ?cnt&:(!= ?cnt ?mcc))
    (mark-mine (id ?id))
    (cell (id ?id) (row ?r) (column ?c))
    (mine (row ?r) (column ?c))

    =>

    (retract ?mc)
    (assert (mine-correct (+ ?cnt 1)))
)

(defrule won

    (declare (salience 25))

    ?f <- (phase check-con)
    (mine-count ?c)
    (mine-correct ?c)

    =>

    (retract ?f)
    (assert (phase won))
    (assert (phase print))
    (assert (print 1))
)

(defrule check-lose

    (declare (salience 20))

    ?f <- (phase check-con)
    (mark-mine (id ?id))
    (cell (id ?id) (row ?r) (column ?c))
    (not (mine (row ?r) (column ?c)))

    =>

    (retract ?f)
    (assert (phase lose))
    (assert (phase print))
    (assert (print 1))
)
(defrule print-detonate

    (declare (salience 10))

    ?f <- (phase print)
    ?p <- (print ?id)
    ?d <- (detonate ?id)
    
    =>

    (retract ?f)
    (retract ?p)
    (retract ?d)
    (assert (print (+ ?id 1)))
    (assert (phase print-space))

    (printout t "x")
)

(defrule print-detonate-eoc

    (declare (salience 20))
    (phase print)
    (size ?s)
    ?p <- (print ?id)
    ?d <- (detonate ?id)
    (cell (id ?id) (column ?c&:(= ?c (- ?s 1))))
    
    =>

    (retract ?p)
    (retract ?d)
    (assert (print (+ ?id 1)))

    (printout t "x" crlf)
)



(defrule print-flag

    (declare (salience 10))

    ?f <- (phase print)
    ?p <- (print ?id)
    (mark-mine (id ?id))
    
    =>

    (retract ?f)
    (retract ?p)
    (assert (print (+ ?id 1)))
    (assert (phase print-space))

    (printout t "f")
)

(defrule print-flag-eoc

    (declare (salience 20))
    (phase print)
    (size ?s)
    ?p <- (print ?id)
    (mark-mine (id ?id))
    (cell (id ?id) (column ?c&:(= ?c (- ?s 1))))
    
    =>

    (retract ?p)
    (assert (print (+ ?id 1)))

    (printout t "f" crlf)
)

(defrule print-space

    ?f <- (phase print-space)

    =>

    (retract ?f)
    (assert (phase print))
    (printout t " ")
)

(defrule print-value

    ?f <- (phase print)
    ?p <- (print ?id)
    (cell-value (id ?id) (value ?v&~-1))
    
    =>

    (retract ?f)
    (retract ?p)
    (assert (print (+ ?id 1)))
    (assert (phase print-space))

    (printout t ?v)
    
)

(defrule print-value-eoc

    (declare (salience 10))

    (phase print)
    (size ?s)
    ?p <- (print ?id)
    (cell-value (id ?id) (value ?v&~-1))
    (cell (id ?id) (column ?c&:(= ?c (- ?s 1))))
    
    =>

    (retract ?p)
    (assert (print (+ ?id 1)))

    (printout t ?v crlf)
    
)

(defrule print-value-nil

    ?f <- (phase print)
    ?p <- (print ?id)
    (cell-value (id ?id) (value ?v&:(= ?v -1)))
    
    =>

    (retract ?f)
    (retract ?p)
    (assert (print (+ ?id 1)))
    (assert (phase print-space))

    (printout t ".")
    
)

(defrule print-value-nil-eoc

    (declare (salience 10))

    (phase print)
    (size ?s)
    ?p <- (print ?id)
    (cell-value (id ?id) (value ?v&:(= ?v -1)))
    (cell (id ?id) (column ?c&:(= ?c (- ?s 1))))
    
    =>

    (retract ?p)
    (assert (print (+ ?id 1)))

    (printout t "." crlf)
    
)

(defrule print-won

    (declare (salience -20))

    (phase print)
    (phase won)

    =>

    (printout t "won" crlf)
)

(defrule print-lose

    (declare (salience -20))

    (phase print)
    (phase lose)

    =>

    (printout t "lose" crlf)
)