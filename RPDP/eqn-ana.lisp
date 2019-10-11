;copyright (c) ELIAS YILMA
(defpackage :eqn-ana
(:use :common-lisp)
(:export #:variable-p #:ex-args #:ex-lhs #:ex-op #:ex-rhs
 #:*ops* #:op-succeedence #:nest-lists #:nest-trigon #:*trig-ops* #:prepare
 #:infix->prefix #:no-unknown #:isolate-var #:solve #:compute #:prefix->infix #:split
 #:*inv-ops* #:inverse-op #:*ops* #:op-succeedence #:nest-lists #:nest-trigon #:*trig-ops* #:prepare
)) 

;++++++++++++++++++++++++LINEAR REGRESSION++++++++++++++++++++++++++++++++++++++++++++
(defun summa(list1)
(cond ((endp list1) 0)
(t (+ (car list1) (summa (rest list1))))))

(defun sxx(list1)
(- (summa  (mapcar #'* list1 list1)) (/ (expt (summa list1) 2) (length list1))))
(defun sxy(list1 list2)
(-  (summa (mapcar #'* list1 list2)) (/ (* (summa list1) (summa list2)) (length list1))))

(defun lin-reg2(list1 list2)
(cond ((not (= (length list1) (length list2))) 
"number of values for the dependent variable is not equal 
to the number of values for the independent variables")
(t (setf beta (/ (sxy list1 list2) (sxx list1)))
(setf alpha (/ (- (summa list2) (* beta (summa list1))) (length list1))) (format t "y = ~a + ~b * x" (* 1.0 alpha) (* 1.0 beta)))))
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++;


;+++++++++++++++++++++++++++equation analysis+++++++++++++++++++++++++++++++++++++++++++++;

;-------------------------isolation of variables-----------------------------------------;
(defun variable-p (exp)
  "Variables are the symbols M through Z."
  (member exp '(x y z m n o p q r s t u v w)))
(defun ex-p (x) (consp x))

(defun ex-args (x) (rest x))

(defun ex-lhs (exp1) (second exp1))

(defun ex-op (exp1) (first exp1))

(defun ex-rhs (exp1) (nth 2 exp1))

(defun mkex (ex-l ex-o ex-r)
(list ex-o ex-l ex-r))
;mktri identify-variables vars-in-exp var-in-exp
(defun mktri (ex-t ex-e)
(list ex-t ex-e))
(defun ex-t(ex)
(first ex))
(defun ex-e(ex)
(second ex))
(defun inverse-trig-op(op)
(second (assoc op *trig-inverses*)))
(defparameter *trig-inverses*
'((sin arcsin) (tan arctan) (cos arccos) (log exp))) 
(defun binary-ex-p (x)
  (and (ex-p x) (= (length (ex-args x)) 2)))
(defparameter variables '(composite-k a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun identify-variables(list1)
(cond 
((null list1) nil)
((numberp list1) nil)
((and (atom list1) (member list1 variables)) list1)
((member list1 variables) list1)
((member (first list1) variables) (cons (first list1) (identify-variables (rest list1))))
((atom (first list1)) (identify-variables (rest list1)))
(t (cons (identify-variables (first list1)) (identify-variables (rest list1))))))

(defun flatten (l)
  (cond ((null l) nil)
		((atom l) l)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defun vars-in-exp (list1)
(flatten (identify-variables list1)))

(defun var-in-exp (x list1)
(cond ((eql x (vars-in-exp list1)) t) ((and (listp list1) (member x (vars-in-exp list1))) t) (t nil)))
(defun one-unknown (list1)
(if (= (length (vars-in-exp list1)) 1) (first (vars-in-exp list1)) nil))
;no-unknown isolate-var solve compute prefix->infix split
;*inv-ops* inverse-op
(defun no-unknown (list1)
(if (null(vars-in-exp list1)) t nil))
  
(defun isolate-var (e x)
(cond 
((eql (ex-lhs e) x) e)

((var-in-exp x (ex-rhs e)) (isolate-var (mkex (ex-rhs e) '= (ex-lhs e)) x))

((unary-ex-p (ex-lhs e)) (isolate-var (mkex (ex-e (ex-lhs e)) '= (mktri (inverse-trig-op (ex-t (ex-lhs e))) (ex-rhs e))) x))

((var-in-exp  x (ex-lhs (ex-lhs e))) 
(isolate-var (mkex (ex-lhs (ex-lhs e)) '= (mkex (ex-rhs e) (inverse-op (ex-op (ex-lhs e))) (ex-rhs (ex-lhs e)))) x))

((commutative-p (ex-op (ex-lhs e))) 
(isolate-var (mkex (ex-rhs (ex-lhs e)) '= (mkex (ex-rhs e) (inverse-op (ex-op (ex-lhs e))) (ex-lhs (ex-lhs e)))) x))

(t (isolate-var (mkex (ex-rhs (ex-lhs e)) '=
(mkex (ex-lhs (ex-lhs e)) (ex-op (ex-lhs e)) (ex-rhs e))) x))))

(defun solve(eqn x)
(isolate-var (infix->prefix eqn) x))

(defun compute(eqn x)
(cond
((not (null (vars-in-exp (ex-rhs (solve eqn x))))) (eval (ex-rhs (solve eqn x))))
(t "unknowns still exist within the eqn"))) 
;inverse rules
(defparameter *inv-ops* '((+ -) (- +) (* /) (/ *)))
(defun inverse-op (op)
(second (assoc op *inv-ops*)))
(defun commutative-p(op)  (member op '(+ - *)))  


;--------------------------------------infix->prefix---------------------------------;
(defun prefix->infix (exp1)
  "Translate prefix to infix expressions."
  (if (atom exp1) exp1
      (mapcar #'prefix->infix
              (if (binary-ex-p exp1)
                  (list (ex-lhs exp1) (ex-op exp1) (ex-rhs exp1))
                  exp1)))) 
(defun unary-ex-p (ex)
(and (listp ex) (= (length ex) 2)))



(defun split (list index)
  (assert (<= 0 index (1- (length list)))
          () "Cannot split before or after the bounds of the list")
  (values (subseq list 0 index)
          (elt list index)
          (subseq list (1+ index))))

;*ops* op-succeedence nest-lists nest-trigon *trig-ops* prepare
;infix->prefix
(defparameter *ops* '(= + - * / sine))

(defun op-succeedence(ex)
(or (position (first *ops*) ex)
(position (second *ops*) ex)
(position (third *ops*) ex)
(position (fourth *ops*) ex)
(position (fifth *ops*) ex)))
  
(defun nest-lists(ex)
(let ((index (op-succeedence ex)))
(cond (index (multiple-value-bind (before op after) (split ex index)
(list (nest-lists before) op (nest-lists after))))
(t (car ex))))) 


(defun nest-trigon(lists)
(cond 
((endp lists) nil)
((not (member (car lists) *trig-ops*)) (cons (car lists) (nest-trigon (cdr lists))))
( (member (car lists) *trig-ops*) 

(if (consp (second lists)) (cons (list (car lists) (nest-lists (second lists))) (nest-trigon (cddr lists)))
(cons (list (car lists) (second lists)) (nest-trigon (cddr lists)))))
(t (nest-trigon (cdr lists)))))

 
(defparameter *trig-ops* '(tan tangent arctan arctangent 
cosine cos arccos arccosine log sin sine arcsine arcsin ))

(defun prepare(ex)
(cond ((unary-ex-p ex) (list (car ex) (infix->prefix (cdr ex))))
((= (length ex) 1) (prepare (car ex))) 
((prefix->infix (nest-lists ex)))
(t "illegal exp")))

(defun infix->prefix(ex)
(prepare (nest-trigon ex)))
 
;------------------------------------------------------------------------------------;
;simultaneous eqn solving
;simplification 
;output print-out
;differentiation and integration
;normalization 
;rule-parsing and writing