(in-package #:core-gp)

;;;
;;; classes
;;;

;;
;; fitness values

(defclass fitness ()
  ((raw-score
    :initarg :raw-score :initform nil
    :accessor raw-score
    :documentation "Value obtained from the evaluation function.")
   (fitness-score
    :accessor fitness-score
    :documentation "Scaled value of raw-score.")))

(defmethod initialize-instance :after ((fitness fitness) &key scaling-function)
  (setf (slot-value fitness 'fitness-score)   
	(if scaling-function
	    (funcall scaling-function (slot-value fitness 'raw-score))
	    (slot-value fitness 'raw-score))))

(defun make-fitness (&key raw-score scaling-function)
  "Create an empty of filled fitness."
  (if raw-score
      (if scaling-function
	  (make-instance  'fitness
			  :raw-score raw-score
			  :scaling-function scaling-function)
	  (make-instance 'fitness :raw-score raw-score))
      (make-instance 'fitness)))

(defmethod copy ((fitness fitness))
  (let ((copy (make-instance 'fitness :raw-score (raw-score fitness))))
    (setf (fitness-score copy) (fitness-score fitness)) copy))

(defmethod print-object ((object fitness) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (raw-score fitness-score) object
      (format stream "raw-score: ~a fitness-score: ~a" raw-score fitness-score)))) 


;;;
;;; methods
;;;

;;
;; fitness values

(defgeneric set-fitness (fitness raw-score evaluation-config)
  (:documentation "Set fitness. Compute its scaled value if scaling is set."))

(defmethod set-fitness ((fitness fitness) raw-score (config evaluation-config))
  (setf (raw-score fitness) raw-score))

(defmethod set-fitness :after ((fitness fitness) raw-score (config evaluation-config))
  (setf (fitness-score fitness) 
	(if (scaling-p config)
	    (funcall (scaling-function config) raw-score)
	    raw-score)))

;; 
;; genome

(defgeneric evaluate-genome (genome evaluation-config)
  (:documentation "Evaluate a genome."))

(defmethod evaluate-genome ((genome genome) (config evaluation-config))
  (funcall (evaluation-function config) (chromossome genome)))

;;
;; individual

(defgeneric evaluate-individual (individual evaluation-config)
  (:documentation "Evaluate an individual."))

(defmethod evaluate-individual ((individual individual) (config evaluation-config))
  (when (eval-p individual)
    (set-fitness (fitness individual) 
		 (evaluate-genome (genome individual) config) config)
    (setf (eval-p individual) nil)))

;; 
;; population

(defgeneric evaluate-population (population evaluation-config)
  (:documentation "Evaluate a population."))

(defmethod evaluate-population ((population population) (config evaluation-config))
  (loop for individual across (individuals population)
     do (evaluate-individual individual config)))


;;;
;;; scaling functions
;;; 

(defun linear-scaling (max-size)
  "Scales the raw fitness between 0 and 1."
  #'(lambda (raw-fitness)
      (/ raw-fitness max-size)))