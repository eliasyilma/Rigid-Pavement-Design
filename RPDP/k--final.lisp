;;;;Copyright (c) 2014 ELIAS YILMA
;to-do ---> gracious exit from the program 
;to-do ---> normalize-projected slab thickness
(defpackage :k--final
(:use :common-lisp )
(:export #:k-value #:ask-sb-thickness #:ask-sg-depth #:ask-sb-los #:ask-pst
 #:ask-sbrm #:ask-rbsrm #:composite-k #:prompt #:new-symbol #:comp-k-vs-fx+gx
 #:*months* #:avg-values #:interpolate-val #:normalize-rbsrm #:normalize-sbrm
 #:modified-k #:normalize-comp-k #:interpolate #:relative-damage #:round-off-pst
 #:deep-replace #:solve-uf-eqn #:interpolate-sg-dep #:effective-k #:normalize-fx+gx #:interpolate-comp-k
 #:interpolate-mod-k #:normalize-sg-depth #:k-top)) 

(defun k-top()
(prog ()
loop
(k-value)
(go loop)))

;k-value 
(defun k-value()
"top-level: runs and evaluates variables and returns k-value"
	;get subbase thickness from the user:type nump
	;(setf sb-thickness (prompt "subbase thickness:"))
    
	;get subgrade depth from the user:type nump
	;(setf sg-depth (prompt "subgrade depth:"))
	;get loss of support value from the user
	;(setf ls (prompt "loss of subbase support:"))
	;get projected slab thickness(PST) from the user:type nump
	;(setf pst (prompt "projected slab thickness:"))
	;(round-off-pst)
	;(format t "MONTH     :   RBSRM ~%")
	;road-bed soil resilient modulus
	;(loop for month in *months* do
       ;(setf (get 'rbsrm month) (prompt "~a   :   " month)))
	;(format t "MONTH     :   SBRM ~%")
	;subbase resilient/elastic modulus
	;(loop for month in *months* do
    ;   (setf (get 'sbrm month) (prompt "~a   :   " month)))
	;(loop for month in *months* do
	(ask-sb-thickness)
	(ask-sg-depth)
	(ask-sb-los)
	(ask-pst)
	(format t "MONTH     :   RBSRM ~%")
	(loop for month in *months* do
	(ask-rbsrm month))
	(format t "MONTH     :   SBRM ~%")
	(loop for month in *months* do
	(ask-sbrm month))
	;compute composite-k and modified k-values
  (loop for month in *months* do
     (let ((rbsrm (get 'rbsrm month)) (sbrm (get 'sbrm month)))
	
	 (setf (get 'composite-k month) (composite-k rbsrm sbrm sb-thickness))
	 (setf (get 'modified-k month) (modified-k rbsrm composite-k sg-depth))
	 (setf (get 'rel-damage month) (relative-damage composite-k pst))))
	;average relative-damage value
(setf sum-uf 0)
  (loop for month in *months* do
       (setf sum-uf (+ sum-uf (get 'rel-damage month))))
  (setf avg-uf (/ sum-uf (length *months*)))
	;effective-k corrected for relative-damage
(setf rel-k-value (solve-uf-eqn pst avg-uf))
	;effective-k corrected fro loss of support
(effective-k ls rel-k-value)
(princ effective-k))



;ask-sb-thickness ask-sg-depth ask-sb-los ask-pst 
(defun ask-sb-thickness()
"prompt for subbase thickness while checking 
for whether the entered value is numerical"
(setf val1 (prompt "~% sb-thickness:"))
(cond ((numberp val1) 
(cond 
((and (>= val1 4.0) (<= val1 20.0)) (setf sb-thickness val1))
(t (princ (concatenate 'string "please enter a value between 4 and 20 inches." '(#\newline))) (ask-sb-thickness))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-sb-thickness))))

(defun ask-sg-depth()
"prompt for subgrade depth while checking 
for whether the entered value is numerical"
(setf val1 (prompt "sg-depth:"))
(cond ((numberp val1) 
(cond 
((and (>= val1 2) (<= val1 10)) (setf sg-depth val1))
(t (princ (concatenate 'string "please enter a value between 2 and 10." '(#\newline))) (ask-sg-depth))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-sg-depth))))

(defun ask-sb-los()
"prompt for loss of support while checking 
for whether the entered value is numerical"
(setf val1 (prompt "loss of subbase support:"))
(cond ((numberp val1) 
(cond 
((and (>= val1 0.0) (<= val1 3.0)) (setf ls val1))
(t (princ (concatenate 'string "please enter a value between 0 and 3 or press e to exit." '(#\newline) "=>")) 
(case (read-char) (#\e) (format t "operation cancelled") (otherwise (ask-sb-los))))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-sb-los))))

(defun ask-pst()
"prompt for subbase thickness while checking 
for whether the entered value is numerical"
(setf val1 (prompt "projected slab thickness:"))
(cond ((numberp val1) 
(cond 
((and (>= val1 6) (<= val1 14)) (setf pst val1) (round-off-pst))
(t (princ (concatenate 'string "please enter an estimated value between 6 and 14 inches." '(#\newline))) (ask-pst))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-pst))))
;ask-sbrm ask-rbsrm composite-k prompt new-symbol comp-k-vs-fx+gx
;*months* avg-values interpolate-val normalize-rbsrm normalize-sbrm 
;modified-k normalize-comp-k interpolate relative-damage round-off-pst
;deep-replace solve-uf-eqn effective-k normalize-fx+gx interpolate-comp-k
;interpolate-mod-k
(defun ask-sbrm(month)
"prompt for subbase thickness while checking 
for whether the entered value is numerical"
(setf val1 (prompt "~a :	" month))
(cond ((numberp val1) 
(cond 
((and (>= val1 15000) (<= val1 1000000)) (setf (get 'sbrm month) val1))
(t (princ (concatenate 'string "please enter a value between 15000 and 1000000." '(#\newline))) (ask-sbrm month))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-sbrm month))))

(defun ask-rbsrm(month)
"prompt for subbase thickness while checking 
for whether the entered value is numerical"
(setf val1 (prompt "~a :	" month))
(cond ((numberp val1) 
(cond 
((and (>= val1 1000) (<= val1 20000)) (setf (get 'rbsrm month) val1))
(t (princ (concatenate 'string "please enter a value between 1000 and 20000." '(#\newline))) (ask-rbsrm month))))
(t (princ (concatenate 'string "please enter a numerical value." '(#\newline)))
(ask-rbsrm month))))
;-------------------------------------------------------------------------


(defun composite-k (RBSRM SBRM sb-thickness)
"evaluates intermediate values (fx gx) from rbsrm(fx) and sbrm(gx) and computes
composite-k value from their sum.

	-------------------------
	| -fx--\	|\			|
	|		\	| \	comp-k	|
	|	x	 \	|  \		|
	|-----------|------------
	|			|\			|
	| --gx--\	| \45		|
	|		 \	|  \		|
	|		  \		\		|
	-------------------------
"
;so that get function can recognize in '*****psi for rbsrm and sbrm values
;eg (get '10000psi ......)
 (let ((~rbsrm (new-symbol rbsrm 'psi)) (~sbrm (new-symbol sbrm 'psi)))
  (setf fx (if (get ~rbsrm 'rbsrm) (eval (get ~rbsrm 'rbsrm)) 
(normalize-rbsrm rbsrm sb-thickness)))
  (setf gx (if (get ~sbrm 'sbrm) (eval (get ~sbrm 'sbrm)) 
(normalize-sbrm sbrm sb-thickness)))
  (setf composite-k (normalize-fx+gx (+ fx gx)))))

(defun prompt (&rest args)
"prompt the user"
  (apply #'format *query-io* args)
  (read *query-io*))

(defun new-symbol(&rest args)
"CREATES A NEW SYMBOL BY JOINING THE VALUES OF THE ARGS"
(intern (format nil "~{~a~}" args)))

(defun comp-k-vs-fx+gx (fx+gx)
"map fx+gx value to a composite-k value"
;(* 4.3094 (exp (* 0.0303 fx+gx)))
(avg-values 'comp-k fx+gx ))

(defvar *months* '(january february march april may june july august september october november december))

(defun avg-values (data-type hy fun1 fun2)
"auxiliary function to find the avg of to function values"
(/ (+ (eval (get fun1 data-type)) (eval (get fun2 data-type))) 2)) 

(defun interpolate-val(data-type hy-val x3 fun1 fun2)
(let ((x1 (eval (get fun1 data-type))) (eval (x2 (get fun2 data-type)))
(y1 (values (intern (string-trim "pci" fun1)))) (y2 (values (intern (string-trim "pci" fun2)))))
(interpolate x1 x2 y1 y2 x3)))

(defun normalize-rbsrm(rbsrm sb-thickness)
"if standard values of rbsrm are not met with, then the non-std value's
fx value will be instantiated to the average of the top & bottom-most 
values the rbsrm lies in"
(if (< rbsrm '1000) (eval (get '1000psi 'rbsrm)) 
(if (and (> rbsrm '1000) (< rbsrm '2000)) (avg-values 'rbsrm sb-thickness '1000psi '2000psi)
(if (and (> rbsrm '2000) (< rbsrm '3000)) (avg-values 'rbsrm sb-thickness'2000psi '3000psi)
(if (and (> rbsrm '3000) (< rbsrm '5000)) (avg-values 'rbsrm sb-thickness'3000psi '5000psi)
(if (and (> rbsrm '5000) (< rbsrm '7000)) (avg-values 'rbsrm sb-thickness '5000psi '7000psi)
(if (and (> rbsrm '7000) (< rbsrm '10000)) (avg-values 'rbsrm sb-thickness '7000psi '10000psi)
(if (and (> rbsrm '10000) (< rbsrm '12000)) (avg-values 'rbsrm sb-thickness '10000psi '12000psi)
(if (and (> rbsrm '12000) (< rbsrm '16000)) (avg-values 'rbsrm sb-thickness '12000psi '16000psi)
(if (and (> rbsrm '16000) (< rbsrm '20000)) (avg-values 'rbsrm sb-thickness '16000psi '20000psi)
'1200))))))))))

(defun normalize-sbrm(sbrm sb-thickness)
"if standard values of sbrm are not met with, then the non-std value's
gx value will be instantiated to the average of the top & bottom-most 
values the sbrm lies in"
(if (< sbrm '15000) (eval (get '15000psi 'sbrm)) 
(if (and (> sbrm '15000) (< sbrm '30000)) (avg-values 'sbrm sb-thickness '15000psi '30000psi)
(if (and (> sbrm '30000) (< sbrm '50000)) (avg-values 'sbrm sb-thickness'30000psi '50000psi)
(if (and (> sbrm '50000) (< sbrm '75000)) (avg-values 'sbrm sb-thickness'50000psi '75000psi)
(if (and (> sbrm '75000) (< sbrm '100000)) (avg-values 'sbrm sb-thickness '75000psi '100000psi)
(if (and (> sbrm '100000) (< sbrm '200000)) (avg-values 'sbrm sb-thickness '100000psi '200000psi)
(if (and (> sbrm '200000) (< sbrm '400000)) (avg-values 'sbrm sb-thickness '200000psi '400000psi)
(if (and (> sbrm '400000) (< sbrm '600000)) (avg-values 'sbrm sb-thickness '400000psi '600000psi)
(if (and (> sbrm '600000) (< sbrm '1000000)) (avg-values 'sbrm sb-thickness '600000psi '1000000psi)
(eval (get '1000000psi 'sbrm))))))))))))

(setf (get '15000psi 'sbrm) '(- (* 49.21 (log sb-thickness)) 43.19))
(setf (get '30000psi 'sbrm) '(- (* 52.73 (log sb-thickness)) 43.2))
(setf (get '50000psi 'sbrm) '(- (* 55.36 (log sb-thickness)) 47.03))
(setf (get '75000psi 'sbrm) '(- (* 56.61 (log sb-thickness)) 46.01))
(setf (get '100000psi 'sbrm) '(- (* 56.51 (log sb-thickness)) 44.37))
(setf (get '200000psi 'sbrm) '(- (* 60.69 (log sb-thickness)) 48.09))
(setf (get '400000psi 'sbrm) '(- (* 63.01 (log sb-thickness)) 47.81))
(setf (get '600000psi 'sbrm) '(- (* 65.11 (log sb-thickness)) 48.99))
(setf (get '1000000psi 'sbrm) '(- (* 67.32 (log sb-thickness)) 50.04))

(setf (get '1000psi 'rbsrm) '(* 102.509 (exp (* -0.126010 sb-thickness))))
(setf (get '2000psi 'rbsrm) '(* 104.336 (exp (* -0.0741556 sb-thickness))))
(setf (get '3000psi 'rbsrm) '(* 111.772 (exp (* -0.0609416 sb-thickness))))
(setf (get '5000psi 'rbsrm) '(* 127.838 (exp (* -0.0530787 sb-thickness))))
(setf (get '7000psi 'rbsrm) '(* 137.659 (exp (* -0.0489196 sb-thickness))))
(setf (get '10000psi 'rbsrm) '(* 147.627 (exp (* -0.0452857 sb-thickness))))
(setf (get '12000psi 'rbsrm) '(* 155.189 (exp (* -0.0443159 sb-thickness))))
(setf (get '16000psi 'rbsrm) '(* 162.556 (exp (* -0.0419548 sb-thickness))))
(setf (get '20000psi 'rbsrm) '(* 168.915 (exp (* -0.0401639 sb-thickness))))

(defun modified-k(road-bed composite-k subgrade-depth)
(let ((~sg-depth (new-symbol subgrade-depth 'ft)) 
	  (~composite-k (new-symbol composite-k 'pci)))
(setf rbsrm (abs (- 20 (/ road-bed 1000))))
  (setf hy 
	(if (get ~sg-depth 'subgrade-depth)
(eval (get ~sg-depth 'subgrade-depth))
(normalize-sg-depth sg-depth rbsrm)))
  (setf modified-k (if (get ~composite-k 'composite-k)
(eval (get ~composite-k 'composite-k))
(normalize-comp-k composite-k hy)))))

(defun normalize-comp-k(comp-k hy-val)
(if (< comp-k 160) '50pci
(if (and (> comp-k 50) (< comp-k 100)) 
(apply #'(lambda (val) (setf x1 50) (setf x2 100) 
(interpolate-mod-k 'composite-k val '50pci '100pci )) (list comp-k))
(if (and (> comp-k 100) (< comp-k 200)) 
(apply #'(lambda (val) (setf x1 100) (setf x2 200) 
(interpolate-mod-k 'composite-k val '100pci '200pci)) (list comp-k))
(if (and (> comp-k 200) (< comp-k 300)) 
(apply #'(lambda (val) (setf x1 200) (setf x2 300) 
(interpolate-mod-k 'composite-k val '200pci '300pci)) (list comp-k))
(if (and (> comp-k 300) (< comp-k 400)) 
(apply #'(lambda (val) (setf x1 300) (setf x2 400) 
(interpolate-mod-k 'composite-k val '300pci '400pci)) (list comp-k))
(if (and (> comp-k 400) (< comp-k 500)) 
(apply #'(lambda (val) (setf x1 400) (setf x2 500) 
(interpolate-mod-k 'composite-k val '400pci '500pci)) (list comp-k))
(if (and (> comp-k 500) (< comp-k 600)) 
(apply #'(lambda (val) (setf x1 500) (setf x2 600) 
(interpolate-mod-k 'composite-k val '500pci '600pci)) (list comp-k))
(if (and (> comp-k 600) (< comp-k 700)) 
(apply #'(lambda (val) (setf x1 600) (setf x2 700) 
(interpolate-mod-k 'composite-k val '600pci '700pci)) (list comp-k))
(if (and (> comp-k 700) (< comp-k 800)) 
(apply #'(lambda (val) (setf x1 700) (setf x2 800) 
(interpolate-mod-k 'composite-k val '700pci '800pci)) (list comp-k))
(if (and (> comp-k 800) (< comp-k 1000)) 
(apply #'(lambda (val) (setf x1 800) (setf x2 1000) 
(interpolate-mod-k 'composite-k val '800pci '1000pci)) (list comp-k))
(if (and (> comp-k 1000) (< comp-k 1200)) 
(apply #'(lambda (val) (setf x1 1000) (setf x2 1200) 
(interpolate-mod-k 'composite-k val '1000pci '1200pci)) (list comp-k))
(if (and (> comp-k 1200) (< comp-k 1400)) 
(apply #'(lambda (val) (setf x1 1200) (setf x2 1400) 
(interpolate-mod-k 'composite-k val '1200pci '1400pci)) (list comp-k))
(eval (get '1400pci 'composite-k )))))))))))))))
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;START HERE
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(setf (get '10ft 'subgrade-depth) '(* 0.113 (expt rbsrm 1.879)))
(setf (get '5ft 'subgrade-depth) '(* 4.178 (exp (* rbsrm 0.131))))
(setf (get '2ft 'subgrade-depth) '(* 9.795 (exp (* 0.109 rbsrm))))

(defun normalize-sg-depth(sg-depth rbsrm)
(if (< sg-depth 2) 50
(if (and (> sg-depth 2) (< sg-depth 5)) 
(apply #'(lambda (val) (setf x1 2) (setf x2 5) 
(interpolate-sg-dep 'subgrade-depth val '2ft '5ft)) (list sg-depth))
(if (and (> sg-depth 5) (< sg-depth 10)) 
(apply #'(lambda (val) (setf x1 5) (setf x2 10) 
(interpolate-sg-dep 'subgrade-depth val '5ft '10ft)) (list sg-depth))
60))))

(setf (get '50pci 'composite-k) '(/ (+ hy 50) 2.4)) 
(setf (get '100pci 'composite-k) '(/ (+ hy 33.68) 0.737))
(setf (get '200pci 'composite-k) '(/ (+ hy 30) 0.275))
(setf (get '300pci 'composite-k) '(/ (+ hy 29.65) 0.15))
(setf (get '400pci 'composite-k) '(/ (+ hy 32.2) 0.107))
(setf (get '500pci 'composite-k) '(/ (+ hy 32.2) 0.078))
(setf (get '600pci 'composite-k) '(/ (+ hy 32) 0.061))
(setf (get '700pci 'composite-k) '(/ (+ hy 30) 0.048))
(setf (get '800pci 'composite-k) '(/ (+ hy 32) 0.041))
(setf (get '1000pci 'composite-k) '(/ (+ hy 28) 0.0275))
(setf (get '1200pci 'composite-k) '(/ (+ hy 34) 0.025))
(setf (get '1400pci 'composite-k) '(/ (+ hy 33) 0.02))

(defun interpolate(x1 x2 y1 y2 x3)
(let ((m (/ (- y2 y1) (- x2 x1))) (xf (- x3 x1)))
(+ (* m xf) y1)))

(defun relative-damage (composite-k PST)
					;PST --> projected slab thickness
(let ((~pst (new-symbol pst 'in)))
  (setf uf (eval (get ~pst 'pst)))))

(setf (get '6in 'pst) '(+ (* -9.39 (log composite-k)) 70.11))
(setf (get '7in 'pst) '(+ (* -14.7 (log composite-k)) 114.6))
(setf (get '8in 'pst) '(+ (* -21.3 (log composite-k)) 172.8))
(setf (get '9in 'pst) '(+ (* -29.2 (log composite-k)) 245.7))
(setf (get '10in 'pst) '(+ (* -37.6 (log composite-k)) 330.2))
(setf (get '12in 'pst) '(+ (* -57.1 (log composite-k)) 540.7))
(setf (get '14in 'pst) '(+ (* -82.7 (log composite-k)) 830.3))

(defun round-off-pst()
	(cond ((< pst 6) (setf pst 6))
		((> pst 14) (setf pst 14))
		(t (setf pst (round pst)))))

(defun deep-replace(replacer replacee list1)
(cond
((endp list1) nil)
((equal replacee (first list1)) (cons replacer (deep-replace replacer replacee (rest list1))))
((atom (first list1)) (cons (first list1) (deep-replace replacer replacee (rest list1))))
(t (cons (deep-replace replacer replacee (first list1)) (deep-replace replacer replacee (rest list1))))))  


(defun solve-uf-eqn(pst avg-uf)
(let ((~pst (new-symbol pst 'in)))
(compute 
(prefix->infix 
(deep-replace 'k 'composite-k (append (cons '= (list (get ~pst 'pst))) (list avg-uf)))) 'k)))

(setf (get '0ls 'ls) '(expt 10 (- (* (log rel-k-value 10) 1.01) 0.010)))
(setf (get '1ls 'ls) '(expt 10 (- (* (log rel-k-value 10) 0.839) 0.067)))
(setf (get '2ls 'ls) '(expt 10 (- (* (log rel-k-value 10) 0.640) 0.075)))
(setf (get '3ls 'ls) '(expt 10 (- (* (log rel-k-value 10) 0.540) 0.178)))

(defun effective-k(ls rel-k-value)
(let ((~ls (new-symbol (values (round ls)) 'ls)))
(setf effective-k (eval (get ~ls 'ls)))))


(setf (get '80u  'comp-k) 50)
(setf (get '100u 'comp-k) 100)
(setf (get '125u 'comp-k) 200)
(setf (get '140u 'comp-k) 300)
(setf (get '150u 'comp-k) 400)
(setf (get '160u 'comp-k) 500)
(setf (get '165u 'comp-k) 600)
(setf (get '173u 'comp-k) 800)
(setf (get '180u 'comp-k) 1000)
(setf (get '190u 'comp-k) 1500)
(setf (get '200u 'comp-k) 2000)


(defun normalize-fx+gx(fx+gx)
"if standard values of fx+gx are not met with, then the non-std value's
fx value will be instantiated to the average of the top & bottom-most 
values the fx+gx lies in"
(if (< fx+gx '80) (eval (get '80u  'comp-k)) 
(if (and (> fx+gx '80) (< fx+gx '100)) 
(apply #'(lambda (val) (setf x1 80) (setf x2 100) 
(interpolate-comp-k 'comp-k  val '80u  '100u)) (list fx+gx))

(if (and (> fx+gx '100) (< fx+gx '125)) 
(apply #'(lambda (val) (setf x1 100) (setf x2 125) 
(interpolate-comp-k 'comp-k  val '100u  '125u)) (list fx+gx))

(if (and (>= fx+gx '125) (< fx+gx '140)) 
(apply #'(lambda (val) (setf x1 125) (setf x2 140) 
(interpolate-comp-k 'comp-k  val '125u  '140u)) (list fx+gx))

(if (and (>= fx+gx '140) (< fx+gx '150)) 
(apply #'(lambda (val) (setf x1 140) (setf x2 150) 
(interpolate-comp-k 'comp-k  val '140u  '150u)) (list fx+gx))

(if (and (>= fx+gx '150) (< fx+gx '160)) 
(apply #'(lambda (val) (setf x1 150) (setf x2 160) 
(interpolate-comp-k 'comp-k  val '150u  '160u)) (list fx+gx))

(if (and (>= fx+gx '160) (< fx+gx '165)) 
(apply #'(lambda (val) (setf x1 160) (setf x2 165) 
(interpolate-comp-k 'comp-k  val '160u  '165u)) (list fx+gx))

(if (and (>= fx+gx '165) (< fx+gx '173)) 
(apply #'(lambda (val) (setf x1 165) (setf x2 173) 
(interpolate-comp-k 'comp-k  val '165u  '173u)) (list fx+gx))

(if (and (>= fx+gx '173) (< fx+gx '180)) 
(apply #'(lambda (val) (setf x1 173) (setf x2 180) 
(interpolate-comp-k 'comp-k  val '173u  '180u)) (list fx+gx))

(if (and (>= fx+gx '180) (< fx+gx '190)) 
(apply #'(lambda (val) (setf x1 180) (setf x2 190) 
(interpolate-comp-k 'comp-k  val '180u  '190u)) (list fx+gx))

(if (and (>= fx+gx '190) (< fx+gx '200)) 
(apply #'(lambda (val) (setf x1 190) (setf x2 200) 
(interpolate-comp-k 'comp-k  val '190u  '200u)) (list fx+gx))

'2000))))))))))))

(defun interpolate-comp-k(data-type val fun1 fun2)
;(interpolate x1 x2 y1 y2 x3)
(interpolate x1 x2 (eval (get fun1 data-type)) (eval (get fun2 data-type)) val))


(defun interpolate-mod-k(data-type val fun1 fun2)
;(interpolate x1 x2 y1 y2 x3)
(interpolate  x1 x2 (eval (get fun1 data-type)) (eval (get fun2 data-type)) val))

(defun interpolate-sg-dep(data-type val fun1 fun2)
;(interpolate x1 x2 y1 y2 x3)
(interpolate  x1 x2 (eval (get fun1 data-type)) (eval (get fun2 data-type)) val))