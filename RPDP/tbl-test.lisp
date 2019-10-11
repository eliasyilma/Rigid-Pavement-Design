;(setf sl-th 9)
;(setf k-value '72pci)
;(setf drai-cond '1hr)
;(setf esal '5.2E06)
;(load "c:/users/user/quicklisp/setup.lisp")
;(ql:quickload "cl-pdf")
;(ql:quickload "cl-typesetting")

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
		(tt:paragraph () (tt:format-string "PROJECTED SLAB THICKNESS = ~a               ESAL = ~a" 1 3))
		(tt:paragraph () (tt:format-string "K-VALUE = ~a" 3))
		(tt:paragraph () (tt:format-string "DRAINAGE TIME = ~a" 5))
			(tt:table (:col-widths '(73 60 60 70 70 60) :border 1)
				(tt:row (:height row-height)
					(tt:cell () "month")
					(tt:cell () "roadbed modulus")
					(tt:cell () "subbase modulus")
					(tt:cell () "composite reaction")
					(tt:cell () "modified reaction")
					(tt:cell () "relative damage"))
			(loop for month in *months* do
				(tt::row (:height row-height)
				(tt:cell ()	(tt:format-string "~a" month))
				(tt:cell ()	(tt:format-string "~a" (get 'rbsrm month)))
				(tt:cell ()	(tt:format-string "~a" (get 'sbrm month)))
			(tt:cell () (tt:format-string "~a" (get 'composite-k month)))
			(tt:cell () (tt:format-string "~a" (get 'modified-k month)))
			(tt:cell () (tt:format-string "~a" (get 'rel-damage month))))
))))
		(tt::draw-pages content :margins margins :break :after)
		(pdf:write-document file))))