(in-package #:core-gp)

;;;
;;; fitness evaluations
;;;

(defun eval-population (population size fitness-function generation)
  "Set the fitness to every element in the population."
  (loop 
     for individual across population
     for id from 1 to size
     do (eval-individual individual fitness-function id generation)))

(defun eval-individual (individual fitness-function id generation)
  "Set the fitness function of a single individual."
  (when (individual-eval-p individual)
    (setf (individual-fitness individual) 
	  (funcall fitness-function individual id generation))
    (setf (individual-eval-p individual) nil)))

