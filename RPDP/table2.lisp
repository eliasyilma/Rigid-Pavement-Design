;(setf sl-th 9)
;(setf k-value '72pci)
;(setf drai-cond '1hr)
;(setf esal '5.2E06)
(load "c:/users/user/quicklisp/setup.lisp")
(ql:quickload "cl-pdf")
(ql:quickload "cl-typesetting")

;(defun prompt (&rest args)
 ; (apply #'format *query-io* args)
 ; (read *query-io*))

(defvar *months* '(january february march april may june july 
august september october november december))

;(defun new-symbol(&rest args)
;"CREATES A NEW SYMBOL BY JOINING THE VALUES OF THE ARGS"
;(intern (format nil "~{~a~}" args)))

;(defun k-value()
;	(setf sb-thickness (new-symbol (prompt "subbase thickness:") 'in))
;    (setf sg-depth (new-symbol (prompt "subgrade depth:") 'ft))
;	(setf pst (new-symbol (prompt "projected slab thickness:") 'in))
;	(format t "MONTH     :   RBSRM ~%")
;  (loop for month in *months* do
;       (setf (get 'rbsrm month) (prompt "~a   :   " month)))
;		(format t "MONTH     :   SBRM ~%")
;  (loop for month in *months* do
;       (setf (get 'sbrm month) (prompt "~a   :   " month))))


(defun test-table (&optional (file "test-table12.pdf")
                   &aux content table (margins '(72 72 72 50)))
  (let* ((row-height nil))
   (tt:with-document ()
    (setq content (tt:compile-text (:font (pdf:get-font "courier")
                                    :font-size 12)
	    (tt:paragraph () "PAVEMENT DESIGN RESULTS")
		(tt:paragraph () (tt:format-string "PROJECTED SLAB THICKNESS = ~a" pst))
		(tt:paragraph () (tt:format-string "K-VALUE = ~a" effective-k))
		(tt:paragraph () (tt:format-string "SUBBASE THICKNESS = ~a" sb-thickness))
		(tt:paragraph () (tt:format-string "LOSS OF SUPPORT = ~a" ls))
		(tt:paragraph () (tt:format-string "DEPTH TO RIGID FOUNDATION = ~a" sg-depth))
			(tt:table (:col-widths '(73 60 60 70 70 60) :border 1)
				(tt:row (:height row-height)
					(tt:cell () "Month")
					(tt:cell () "Roadbed Modulus")
					(tt:cell () "Subbase Modulus")
					(tt:cell () "Composite Reaction")
					(tt:cell () "Modified Reaction")
					(tt:cell () "Relative Damage"))
			(loop for month in *months* do
				(tt::row (:height row-height )
				(tt:cell (:row-span 2)	(tt:format-string "~a" month))
				(tt:cell ()	(tt:format-string "~a" (get 'rbsrm month)))
				(tt:cell ()	(tt:format-string "~a" (get 'sbrm month)))
			(tt:cell () (tt:format-string "~a" (get 'composite-k month)))
			(tt:cell () (tt:format-string "~a" (get 'modified-k month)))
			(tt:cell () (tt:format-string "~a" (get 'rel-damage month))))
				(tt::row (:height row-height )
			(tt:cell ()	(tt:format-string "-" ))
			(tt:cell ()	(tt:format-string "-" ))
			(tt:cell ()	(tt:format-string "-" ))
			(tt:cell ()	(tt:format-string "-" ))
			(tt:cell ()	(tt:format-string "-" ))))
	(tt:row (:height row-height)
	(tt:cell (:col-span 5) (tt:format-string "SUMMATION OF RELATIVE DAMAGE, S(uf)= ~a" sum-uf))
	(tt:cell () (tt:format-string "~a" 'r))))
		
		(tt:paragraph () (tt:format-string "Average: SUM(UF)/NO. OF MONTHS = ~a" avg-uf))
		(tt:paragraph () (tt:format-string "EFFECTIVE MODULUS OF SUBGRADE REACTION, k(pci) = ~a" rel-k-value))
		(tt:paragraph () (tt:format-string "CORRECTED FOR LOSS OF SUPPORT, k(pci)= ~a" effective-k))
(tt:table (:col-widths '(250 70) :border 1)
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "PARAMETERS"))
					(tt:cell () (tt:format-string "VALUES")))		
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "EQUIVALENT SINGLE AXLE LOAD,W18"))
					(tt:cell () (tt:format-string "~a" w18)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "RELIABILITY,R"))
					(tt:cell () (tt:format-string "~a" reliability)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "OVERALL STANDARD DEVIATION,So" ))
					(tt:cell () (tt:format-string "~a" std-nor-dev)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "DESIGN SERVICEABILITY LOSS,PSI"))
					(tt:cell () (tt:format-string "~a" psi)))					
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "CONCRETE ELASTIC MODULUS,Ec"))
					(tt:cell () (tt:format-string "~a" ec)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "CONCRETE MODULUS OF RUPTURE,Sc"))
					(tt:cell () (tt:format-string "~a" sc)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "LOAD TRANSFER COEFFICIENT,J"))
					(tt:cell () (tt:format-string "~a" j-factor)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "K-VALUE,k"))
					(tt:cell () (tt:format-string "~a" effective-k)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "SLAB LENGTH,L"))
					(tt:cell () (tt:format-string "~a" l)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "STEEL WORKING STRESS,Fs"))
					(tt:cell () (tt:format-string "~a" fs)))
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "FRICTION FACTOR,F"))
					(tt:cell () (tt:format-string "~a" f))))

(tt:table (:col-widths '(70 100 100 100) :border 1)
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "trial D"))
					(tt:cell () (tt:format-string "left hand side"))
					(tt:cell () (tt:format-string "right hand side"))
					(tt:cell () (tt:format-string "difference")))
				(loop for i from 0 to (- (length a) 1) do
				(tt:row (:height row-height)
					(tt:cell () (tt:format-string "~a") (first (nth i a)))
					(tt:cell () (tt:format-string "~a") (second (nth i a)))
					(tt:cell () (tt:format-string "~a") (third (nth i a))))))
(tt:paragraph () (tt:format-string "SLAB THICKNESS, D = ~a inches" d))
(tt:paragraph () (tt:format-string "reinforcement % = LF/2Fs = ~a %" rein-%))
(tt:paragraph () (tt:format-string "reinforcemnt area = 12 * D * % =~a sq.in." rein-a))
))
(tt::draw-pages content :margins margins :break :after)
		(pdf:write-document file))))

