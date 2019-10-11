;(defpackage :rein
;(:use :common-lisp )
;(:export #:rein-% #:fs))

;reinforcement
(defun rein-% ()
;(setf f (prompt "friction factor: "))
;(setf fs (prompt "steel working stress: "))
;(setf l (prompt "length of slab: "))
(?- (steel reinforcement friction factor ?x))
(setf f (aref *q-ans* 0))
(vector-pop *q-ans*)
(?- (steel stress working psi ?x))
(setf fs (aref *q-ans* 0))
(vector-pop *q-ans*)
(?- (concrete slab length ft ?x))
(setf l (aref *q-ans* 0))
(vector-pop *q-ans*)
(?- (concrete slab thickness in ?x))
(setf d (aref *q-ans* 0))
(vector-pop *q-ans*)
(setf rein-% (* (/ (* f l) (* 2 fs)) 100))
(setf rein-a (* 12 rein-% d .01))
(eval `(f (steel reinforcement quantity percentage ,rein-%)))
)


;steel stress
(defun fs()
(setf fs (* 0.75 fy)))

(defvar *layers* '(subgrade subbase))
(defvar *parameters* '(type thickness))
(defvar *materials* '(pcc steel))


(defun check-material-data()
  (loop for month in *months* do
      (cond ((null (get 'rbsrm month))
	    (if (> 15000 (setf a (prompt "please enter rbrsm for the month of ~a ~%>>>" month)))
	     	(setf (get 'rbsrm month) a)
	     	(setf (get 'rbsrm month) 15000)))
	    (t (format t "."))))
  (loop for layer in *layers* do
	(loop for para in *parameters* do
	      (cond ((null (get para layer))
		    (if (equal 'unknown (setf a (prompt "~% what is the ~a of ~a used? ~%>>>" para layer)))
			      (setf (get para layer) (get 'default para))
			   	(setf (get para layer) a)))
		    (t (format t "..")))))
  (loop for material in *materials* do
	(cond ((null (get 'grade material))
	      (setf a (prompt "what is the grade of ~a used? ~%>>>" material)))
	      (t nil))))

