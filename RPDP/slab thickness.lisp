
(defpackage :slab
(:use :common-lisp )
 (:export  #:dd #:dl #:g #:cd #:drain
 #:sc #:k1 #:k2 #:k3 #:k4 #:k5 #:k6 #:k7 #:k8 #:k9 #:slab-thickness #:solve-thickness 
 #:psi->kpa #:kpa->psi #:in->cm #:cm->in #:ft->m #:m->ft #:slab-top-level
 #:w18 #:reliability  #:std-nor-dev #:*r-vs-zr* #:default #:psi #:ec))

;**************************************************************
					  ;DEFAULT VALUES
;**************************************************************
(setf dd 0.50)

(defun dd(value)
(setf dd value))

(setf dl 0.70)

(defun dl(value)
(setf dl value))

(setf g  0.05)

(defun g(value)
(setf g value))

(setf cd 1.00)

(defun drain(conditions)
(setf cd (get conditions 'drainage)))

(setf reliability 0.90)
(setf std-nor-dev -1.282)
(setf psi 2.00)
(setf tsl 2.5)
(setf ec 5000000)
(setf sc 650)

;++++++++++++++++++++++++++++++example+++++++++++++++++++++++++
;k=72pci
;ec=5 * 10^6 psi
;s'c=650psi
;j=3.2
;cd=1.0
;So=0.29
;del.psi=1.7
;r=95% (zn=-1.645)
;w18 = 5.1 * 10^6
(setf esal 14571428.57)
(setf ec 5000000)
(setf period 20)
(setf g 0.04)
(setf k-value 72)

(defun prompt (&rest args)
"prompt the user"
  (apply #'format *query-io* args)
  (read *query-io*))
 
;**************************************************************
					   ;PARAMETERS
;**************************************************************
;traffic 
(defun esal()
(setf esal (* w18-orig (/ (- (expt (+ 1 g) period) 1) g)))) 

(defun w18()
(setf w18 (* dl dd esal)))
;reliability
(setf (get 'i&of 'urban) 0.925)    (setf (get 'i&of 'rural) 0.9)
(setf (get 'pa 'urban) 0.9)        (setf (get 'pa 'rural) 0.85)
(setf (get 'coll 'urban) 0.875)    (setf (get 'coll 'rural) 0.85)
(setf (get 'loc 'urban) 0.650)     (setf (get 'loc 'rural) 0.65)

(defun reliability(road-funct road-loc)
(setf reliability 
(get road-funct road-loc)))

;w18-orig period 

;standard normal deviate
(defun std-nor-dev(rel)
(setf std-nor-dev (second (assoc rel *r-vs-zr*))))

(defparameter *r-vs-zr*
'((0.925 -1.141) 
(0.90 -1.282) 
(0.875 -1.1595) 
(0.85 -1.037) 
(0.65 -0.3885)))

;overall standard deviation
(setf so 0.35)
;serviceability loss
(setf (get 'default 'tsl) 2.5)
(setf (get 'default 'isi) 4.5)

(defun default(item)
(get 'default item))

(defun psi(po pt)
(setf psi (- po pt)))

;concrete elastic modulus
(defun ec()
(setf ec (* 57000 (sqrt fc))))
;concrete modulus of rupture
(defun sc(fc)
(setf sc (* 7.5 (sqrt fc))))
;j-factor
(setf j-factor 3.2)
;drainage
(setf (get 'excellent 1.15) 'drainage)
(setf (get 'good 1.0875) 'drainage)
(setf (get 'fair 1) 'drainage)
(setf (get 'poor 0.9) 'drainage)
(setf (get 'very-poor 0.8) 'drainage)
;******************************************************************
;EVALUATE THE EQUATION
(defun k1 ()
(setf k1 (log (w18) 10)))

(defun k2()
(setf k2 (* std-nor-dev so)))


(defun k3()
(setf k3 (* 7.35 (log (+ D 1) 10))))

(defvar k4 0.06)

(defun k5()
(setf k5 (log (/ psi (- 4.5 1.5)) 10)))

(defun k6()
(setf k6 (- 4.22 (* 0.32 tsl))))

(defun K7()
(setf k7 (log (/ (* sc cd) (* 215.63 j-factor)) 10)))

;k-value is needed at k8.

(defun k8()
(setf k8 
(log (/ (- (expt d 0.75) 1.132) (- (expt d 0.75) (/ 18.42 (expt (/ ec k-value) 0.25)))) 10)))

(defun k9()
(setf k9 (+ 1 (/ 1.624e7 (expt (+ d 1) 8.46)))))

(defun slab-thickness()
(+ (k2) (k3) k4 (/ (k5) (k9)) (* (k6) (+ (k7) (k8))))) 

(defvar *std-thicknesses* '(5 6 7 8 9 10 11 12))

(defun solve-for-d()
(format t "log18     right-side        delta ~%")
(loop for std-thickness in '(5 6 7 8 9 10 11 12 13 15 17) do
(setf d std-thickness)
(k1)
(let ((slab-thickness (slab-thickness)))
(format t "~a ---- ~a ---- ~a ~%" 
k1 slab-thickness (- k1 slab-thickness)))))

(defun solve-thickness()
(k1)
(setf a '())
(setf d 5)
(setf slab-thickness 4)
(format t "log18         right-side         delta ~%")
(loop until (< (- k1 slab-thickness) 0)
do 
(setf d (+ 1 d))
(setf slab-thickness (slab-thickness))
(setf a (cons (list d k1 slab-thickness) a)) 
(format t "~a ---- ~a ---- ~a ~%" 
k1 slab-thickness (- k1 slab-thickness)))
(setf a (nreverse a)))
 
;+++++++++++++++++++++conversion of units++++++++++++++++++++++

(defun psi->kpa(psi)
(* 6.894757 psi))

(defun kpa->psi(kpa)
(/ kpa 6.894757))

(defun in->cm(in)
(* 2.54 in))

(defun cm->in(cm)
(/ cm 2.54 )) 

(defun ft->m(ft)
(* 0.3048 ft))

(defun m->ft(m)
(/ m 0.3048))
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;initiate k-value
;generate k-value report
;ask for esal
;get the k-value 
;ask for fc

(defun slab-top-level()
(k-value)
;(setf k-value (prompt "k-value: "))
(setf esal (prompt "~% ESAL : "))
(setf k-value effective-k)
(setf fc (prompt "~% compressive strength of concrete :"))
(solve-thickness)
(rein-%)
(setf file (prompt "file name:  "))
(test-table (first file))
(output))

(defun output()
(format t "~%K-VALUE : ~a pci" k-value)
(format t "~%SLAB THICKNESS : ~a in" d)
(format t "~%REINFORCEMENT PERCENTAGE: ~a%" rein-%)
(format t "~%REINFORCEMENT AREA PER UNIT WIDTH: ~asq.in." rein-a))

 
 