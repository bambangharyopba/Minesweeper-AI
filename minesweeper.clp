;;; #######################
;;; DEFTEMPLATES & DEFFACTS
;;; #######################

;; Cell Template
(deftemplate cell
    (slot id) ;; cell id
    (slot row)
    (slot column)
)

;; Value of Cell Template
(deftemplate cell-value
    (slot id) ;; cell id
    (slot value)
)

;; Cell Check Statement
(deftemplate check-cell
    (slot row)
    (slot column)
)

;; Selected Cell
(deftemplate move
    (slot id) ;; cell id
)

;; Available Move
(deftemplate move-pool
    (multislot id) ;; cell id
)

;; Last Used Move
(deftemplate last-move
    (slot id) ;; cell id
)

(deftemplate last-sub-move
    (slot id)
)

(deftemplate sub-move
    (slot id)
)

;; Flagged Mine
(deftemplate mark-mine
    (slot id) ;;; cell id
)

;; Safe Marked Cell
(deftemplate mark-safe
    (slot id) ;;; cell id
)

;; Sentence Cell (ex: {A, B, C, D}) used to make sentence
(deftemplate sentence-cell
    (multislot id) ;; cell ids
)

(deftemplate sub-sentence-cell
    (multislot id)
)

;; Sentence (ex: {A, B, C} = 2)
(deftemplate sentence
    (multislot id) ;; cell ids
    (slot count) ;; mine-count  
)

;; Problem Generated Mine
(deftemplate mine
    (slot row)
    (slot column)
)

;;; #########################
;;; INITIALIZATION COMPONENTS
;;; #########################
;;; **********
;;; initialize
;;; **********

(defrule initialize

    =>

    (assert (phase read-file))

    (printout t "Input File Name" crlf)
    (assert (filename (read)))
)

;;; *****************
;;; read problem file
;;; *****************

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
        (assert (mine  (row (nth$ 1 (explode$ (sub-string 1 1 ?d)))) (column (nth$ 1 (explode$ (sub-string 4 4 ?d))))))
        (bind ?d (readline data)))

    (close data)
)

;;; **************
;;; generate board
;;; **************

(defrule generate-board

    ?f <- (phase generate-board)
    (size ?s)
    (mine-count ?mc)

    =>

    (retract ?f)
    (assert (phase move-start))

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

;;; ###############
;;; MOVE COMPONENTS
;;; ###############
;;; **********
;;; first move
;;; **********

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

;;; ******************************
;;; safe move AKA select safe marked cell
;;; ******************************

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

;;; *********************************************************
;;; probability move, used when there are no safe marked cell
;;; *********************************************************

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

;;; ################
;;; PROBE COMPONENTS
;;; ################
;;; **************************************************
;;; check selected cell if selected cell contains mine
;;; **************************************************

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

;;; *******************************************************
;;; make cell check statement for cell around selected cell
;;; *******************************************************

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

;;; ****************************************
;;; delete out of board cell check statement
;;; ****************************************

(defrule check-filter

    (declare (salience 10))

    (phase check-value)
    (size ?s)
    (or (or (check-cell (row ?r&:(< ?r 0)) (column ?c)) (check-cell (row ?r&:(>= ?r ?s)) (column ?c))) (or (check-cell (row ?r) (column ?c&:(< ?c 0))) (check-cell (row ?r) (column ?c&:(>= ?c ?s)))))
    ?f <- (check-cell (row ?r) (column ?c))

    =>

    (retract ?f)

)

;;; ************************************************************************
;;; add value if checked cell contains mine and retract cell check statement
;;; ************************************************************************

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

;;; ****************************************************************
;;; retract cell chcek statement if checked cell doesnt contain mine
;;; ****************************************************************

(defrule check-value

    (phase check-value)
    ?cc <- (check-cell (row ?r) (column ?c))
    ?sc <- (sentence-cell (id $?sid))
    (cell (id ?id) (row ?r) (column ?c))

    =>

    (retract ?cc)
    (modify ?sc (id (insert$ (create$ $?sid) 1 ?id)))
)

;;; *************
;;; make sentence
;;; *************

(defrule end-check

    (declare (salience -5))

    ?f <- (phase check-value)
    (not (check-cell (row $?) (column $?)))
    ?sc <- (sentence-cell (id $?sid))

    (last-move (id ?lid))
    (cell-value (id ?lid) (value ?v))

    =>

    (retract ?f)
    (retract ?sc)
    (if (= ?v 0)
        then
        (loop-for-count (?cnt 1 (length$ ?sid)) do
            (assert (sub-move (id (nth$ ?cnt ?sid))))
        )
        (assert (last-sub-move (id -1)))
        (assert (phase sub-move))
        else
        (assert (phase update-knowledge))
    )

    (assert (sentence (id ?lid) (count 0)))
    (assert (sentence (id $?sid) (count ?v)))

)

;;; ###########################
;;; Under construction
;;; ##########################

(defrule sub-move

    ?f <- (phase sub-move)
    ?sm <- (sub-move (id ?id))
    ?lsm <- (last-sub-move (id $?))
    ?mp <- (move-pool(id $?mpid))
     
    =>

    (retract ?f)
    (retract ?sm)

    (modify ?mp (id (delete-member$ (create$ $?mpid) ?id)))

    (modify ?lsm (id ?id))
    (assert (move (id ?id)))
    (assert (phase sub-probe-cell))    
)

(defrule sub-probe-cell-value

    ?f <- (phase sub-probe-cell)
    (last-sub-move (id ?id))
    (cell (id ?id) (row ?r) (column ?c))
    ?v <- (cell-value (id ?id))

    =>

    (retract ?f)
    (assert (phase sub-check-value))

    (modify ?v (value 0))
    (assert (sub-sentence-cell))
    
    (loop-for-count (?cnt -1 1) do
        (loop-for-count (?cnt2 -1 1) do
            (if (not (and (= ?cnt 0) (= ?cnt2 0)))
                then
                (assert (check-cell (row (+ ?r ?cnt)) (column (+ ?c ?cnt2))))
            )
        )   
    )
)


(defrule sub-check-filter

    (declare (salience 10))

    (phase sub-check-value)
    (size ?s)
    (or (or (check-cell (row ?r&:(< ?r 0)) (column ?c)) (check-cell (row ?r&:(>= ?r ?s)) (column ?c))) (or (check-cell (row ?r) (column ?c&:(< ?c 0))) (check-cell (row ?r) (column ?c&:(>= ?c ?s)))))
    ?f <- (check-cell (row ?r) (column ?c))

    =>

    (retract ?f)

)

(defrule sub-check-value-mine

    (declare (salience 5))

    (phase sub-check-value)
    ?cc <- (check-cell (row ?r) (column ?c))
    ?sc <- (sub-sentence-cell (id $?sid))
    (mine (row ?r) (column ?c))
    (cell (id ?id) (row ?r) (column ?c))

    (last-sub-move (id ?lid))
    ?cv <- (cell-value (id ?lid) (value ?v))

    =>

    (retract ?cc)
    (modify ?sc (id (insert$ (create$ $?sid) 1 ?id)))
    (modify ?cv (value (+ ?v 1)))

)

(defrule sub-check-value

    (phase sub-check-value)
    ?cc <- (check-cell (row ?r) (column ?c))
    ?sc <- (sub-sentence-cell (id $?sid))
    (cell (id ?id) (row ?r) (column ?c))

    =>

    (retract ?cc)
    (modify ?sc (id (insert$ (create$ $?sid) 1 ?id)))
)

(defrule sub-end-check

    (declare (salience -5))

    ?f <- (phase sub-check-value)
    (not (check-cell (row $?) (column $?)))
    ?sc <- (sub-sentence-cell (id $?sid))
    (last-sub-move (id ?lid))
    (cell-value (id ?lid) (value ?v))
    (or (sub-move (id $?)) (cell-value (id ?lid) (value ?v&:(= ?v 0))))
    ?mp <- (move-pool (id $?mpid))


    =>

    (retract ?f)
    (retract ?sc)
    (if (= ?v 0)
        then
        (loop-for-count (?cnt 1 (length$ ?sid)) do
            (if (subsetp (create$ (nth$ ?cnt ?sid)) $?mpid)
                then
                (assert (sub-move (id (nth$ ?cnt ?sid))))
            )
        )
    )
    (assert (phase sub-move))

    (assert (sentence (id ?lid) (count 0)))
    (assert (sentence (id $?sid) (count ?v)))

)

(defrule sub-end
    
    ?f <- (phase sub-move)
    (not (sub-move (id $?)))

    =>

    (retract ?f)

    (assert (phase update-knowledge))

)

(defrule end-sub-move

    (declare (salience -5))

    ?f <- (phase sub-check-value)
    (not (check-cell (row $?) (column $?)))
    ?sc <- (sub-sentence-cell (id $?sid))
    (not (sub-move (id $?)))
    ?lsm <- (last-sub-move (id ?lid))

    (cell-value (id ?lid) (value ?v))

    =>

    (retract ?f)
    (retract ?sc)
    (retract ?lsm)

    (assert (phase update-knowledge))

    (assert (sentence (id ?lid) (count 0)))
    (assert (sentence (id $?sid) (count ?v)))

)

;;; ################################
;;; KNOWLEDGE ACQUISITION COMPONENTS
;;; ################################
;;; *********************************************************************
;;; sentence subtraction (ex: ({A, B, C} = 2) - ({B, C} = 1) = ({A} = 1))
;;; *********************************************************************

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

;;; ************************************************************
;;; atomize safe sentence (ex: ({A, B} = 0) -> {A} = 0, {B} = 0)
;;; ************************************************************

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

;;; ************************************************************
;;; atomize mine sentence (ex: ({A, B} = 2) -> {A} = 1, {B} = 1)
;;; ************************************************************

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

;;; **********************
;;; mark A safe if {A} = 0
;;; **********************

(defrule atom-safe

    (declare (salience 5))
    (phase update-knowledge)
    ?s <- (sentence (id $?id&:(= (length$ (create$ $?id)) 1)) (count ?cnt&:(= ?cnt 0)))
    (not (mark-safe (id ?idm&:(= ?idm (nth$ 1 $?id)))))

    =>

    (assert (mark-safe (id (nth$ 1 $?id))))
)

;;; **************************************************
;;; mark A mine if {A} = 1 and make move A unavailable
;;; **************************************************

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

;;; ****************************************
;;; end of one move, transition to next move
;;; ****************************************

(defrule end-sentence-subtraction
    
    ?f <- (phase update-knowledge)

    =>

    (retract ?f)
    (assert (phase trans-move))
)

;;; #####################
;;; TRANSITION COMPONENTS
;;; #####################
;;; ******************************************
;;; check board condition if no move available
;;; ******************************************

(defrule check-con

    (declare (salience 20))

    ?f <- (phase trans-move)
    (move-pool (id $?id&:(= (length$ (create$ $?id)) 0)))

    =>

    (retract ?f)
    (assert (phase check-con))

    (assert (mine-correct 0))
)

;;; *************************
;;; check flagged mine = mine
;;; *************************

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

;;; *********************************************
;;; if count(flagged mine) = count(mine) then win
;;; *********************************************

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

;;; *********************************
;;; if flagged mine != mine then lose
;;; *********************************

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

;;; ****************************************
;;; if nothing happen proceed into next move
;;; ****************************************

(defrule trans-move

    ?f <- (phase trans-move)

    =>

    (retract ?f)
    ; (assert (phase move))
    (assert (phase print))
    (assert (print 1))
)

;;; ################
;;; PRINT COMPONENTS
;;; ################
;;; ********************
;;; print detonated mine
;;; ********************

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

;;; **********************************
;;; print detonated mine on end of row
;;; **********************************

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

;;; ******************
;;; print flagged mine
;;; ******************

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

;;; ********************************
;;; print flagged mine on end of row
;;; ********************************

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

;;; ***********
;;; print space
;;; ***********

(defrule print-space

    ?f <- (phase print-space)

    =>

    (retract ?f)
    (assert (phase print))
    (printout t " ")
)

;;; ****************
;;; print cell value
;;; ****************

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

;;; *********************************
;;; print cell value on end of row
;;; *********************************

(defrule print-value-eor

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

;;; ***********************
;;; print if cell value nil
;;; ***********************

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

;;; *************************************
;;; print on end of row if cell value nil
;;; *************************************

(defrule print-value-nil-eor

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

(defrule print-end

    ?f <- (phase print)
    ?p <- (print $?)

    =>
    
    ; (retract ?f)
    (retract ?p)
    
    (printout t crlf)

    (assert (phase move))

)

;;; **************
;;; print won text
;;; **************

(defrule print-won

    (declare (salience -20))

    (phase print)
    (phase won)

    =>

    (printout t "won" crlf)
)

;;; **************
;;; print lose text
;;; **************

(defrule print-lose

    (declare (salience -20))

    (phase print)
    (phase lose)

    =>

    (printout t "lose" crlf)
)