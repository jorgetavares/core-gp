;(in-package #:core-gp)

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
    :initarg :fitness-score
    :accessor fitness-score
    :documentation "Scaled value of raw-score.")))

(defmethod initialize-instance :after ((fitness fitness) &key scaling-function)
  (if scaling-function
      (setf (slot-value fitness 'fitness-score)
	    (funcall scaling-function (slot-value fitness 'raw-score)))
      nil))

(defun make-fitness (&key raw-score scaling-function)
  "Create an empty of filled fitness."
  (if raw-score
      (if scaling-function
	  (make-instance  'fitness
			  :raw-score raw-score
			  :scaling-function scaling-function)
	  (make-instance 'fitness :raw-score raw-score))
      (make-instance 'fitness)))

(defgeneric copy (fitness)
  (:documentation "Return a new identical object to fitness."))

(defmethod copy ((fitness fitness))
  (make-instance 'fitness 
		 :raw-score (raw-score fitness)
		 :fitness-score (fitness-score fitness)))


;;
;; evaluation configuration

(defclass evaluation-config ()
  ((evaluation-function 
    :initarg :evaluation-function 
    :initform (error "Class evaluation-config: must provide an evaluation function.")
    :reader evaluation-function
    :documentation "Evaluation function (required).")
   (scaling-function 
    :initarg :scaling-function :initform nil
    :reader scaling-function
    :documentation "Scaling function (optional).")
   (scaling-p 
    :reader scaling-p
    :documentation "Indicates if scaling is set.")))

(defmethod initialize-instance :after ((config evaluation-config) &key)
  (if (slot-value config 'scaling-function)
      (setf (slot-value config 'scaling-p) t)
      (setf (slot-value config 'scaling-p) nil)))

(defun make-evaluation-config (evaluation-function &optional scaling-function)
  "Return an evaluation configuration."
  (make-instance 'evaluation-config
		 :evaluation-function evaluation-function
		 :scaling-function scaling-function))
  

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
  (when (scaling-p config)
    (setf (fitness-score fitness) 
	  (funcall (scaling-function config) raw-score))))

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
