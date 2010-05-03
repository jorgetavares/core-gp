;;;;
;;;; core-gp example: symbolic regression
;;;;

(in-package #:core-gp-examples)


;;;
;;; function and terminal sets
;;;

(defparameter *X* 0)

(defun var-x () 
  *X*)

(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))

(setf core-gp:*generate-constant* (int-constants -5 5))

(defparameter *fset* (core-gp:make-fset 'core-gp:gp-plus 2
					'core-gp:gp-minus 2 
					'core-gp:gp-times 2
					'core-gp:gp-divison 2
					))

(defparameter *tset* '(core-gp:gp-constant var-x))


;;;
;;; fitness function ( y = f(x) = x^2 / 2 )
;;;

(defparameter *x-points* (loop for i from 0 below 10 collect (/ i 10)))
(defparameter *y-points* (loop for x in *x-points* collect (float (/ (expt x 2) 2))))

(defun make-fitness-regression (fitness-cases x-points y-points)
  #'(lambda (individual id generation)
      (declare (ignore id generation))
      (loop for i from 0 below fitness-cases
	 do (setf *X* (nth i x-points))
	 sum (expt (- (eval (core-gp:individual-tree individual)) 
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
								    10 *x-points* *y-points*)
							  :elitism nil
							  :type :steady-state
							  ))

(defun regression (&key (params *regression-params*) (runs 1)  (output :screen))
  (core-gp:launch-gp *fset* *tset* :params params :runs runs :output output))
