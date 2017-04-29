;;;;
;;;; core-gp example: symbolic regression
;;;;

(defpackage #:core-gp-regression
  (:use #:common-lisp #:core-gp)
  (:export #:regression 
	   #:var-x
	   #:int-constants
	   #:make-fitness-regression
	   #:*X*
	   #:*fset*
	   #:*tset*
	   #:*fitness-cases*
	   #:*x-points*
	   #:*y-points*))

(in-package #:core-gp-regression)


;;;
;;; function and terminal sets
;;;

;; variable
(defparameter *X* 0)
(defnode var-x (() (:string "X" :ephemeral nil)) *X*)

;; constants
(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))


;; functions and terminals
(defparameter *fset* '(gp-plus gp-minus gp-times gp-division))
(defparameter *tset* '(gp-constant var-x))


;;;
;;; fitness function ( y = f(x) = x^2 / 2 )
;;;

(defparameter *fitness-cases* 10)

(defparameter *x-points* (loop for i from 0 below *fitness-cases* 
			    collect (/ i *fitness-cases*)))
(defparameter *y-points* (loop for x in *x-points* 
			    collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases x-points y-points)
  #'(lambda (candidate-solution)
      (loop for i from 0 below fitness-cases
	 do (setf *X* (nth i x-points))
	 sum (expt (- (eval candidate-solution) 
		      (nth i y-points)) 2))))


;;;
;;; run GP
;;;

(defun regression (&key (id "gp-regression") (output :screen) (pop-size 600) (generations 10))
  (setf core-gp:*generate-constant* (int-constants -5 5))
  (core-gp:gp-generic :id id
		      :output output
		      :pop-size pop-size
		      :fset-names *fset*
		      :tset-names *tset*
		      :initial-size 1
		      :maximum-size 4
		      :evaluation-fn (make-fitness-regression 
				      *fitness-cases* *x-points* *y-points*)
		      :elitism nil
		      :replacement-mode :steady-state
		      :terminal-value generations))
