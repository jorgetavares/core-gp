;;;;
;;;; core-gp example: symbolic regression
;;;;

(in-package #:core-gp-examples)


;;;
;;; function and terminal sets
;;;

(defparameter *X* 0)
(defnode var-x (() (:string "X" :ephemeral nil)) *X*)

(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))
(setf core-gp:*generate-constant* (int-constants -5 5))

(defparameter *fset* (core-gp:make-set 'core-gp:gp-plus
				       'core-gp:gp-minus 
				       'core-gp:gp-times
				       'core-gp:gp-divison
				       ))

(defparameter *tset* '(core-gp:gp-constant var-x))


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


(defparameter *regression-params* (core-gp:make-gp-params :total-generations 10
							  :pop-size 1000
							  :initial-depth 1
							  :max-depth 4
							  :fset *fset*
							  :tset *tset*
							  :fitness (make-fitness-regression 
								    *fitness-cases* 
								    *x-points* *y-points*)
							  :elitism nil
							  :type :steady-state
							  ))

(defun regression (&key (params *regression-params*) (runs 1)  (output :screen))
  (loop for run from 1 to runs
     collect (core-gp:launch-gp run *fset* *tset* :params params :output output)))
