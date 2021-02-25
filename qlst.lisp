(defparameter l '(1 2 3 4 5 6 7 8 9 10))

(defparameter l1 nil)
;; l2 is a copy of list l
(setf l2 (copy-list l))
;; l2 and l are NOT the SAME list (see later rplaca changes only l2)
(eq l2 l)
;; l2 and l contain the SAME (copied) elements. It is DEEP copy
(equal l2 l)
;; Only l2 will change, l will remain same
(rplaca l2 11)
