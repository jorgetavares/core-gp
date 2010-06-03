
(in-package #:core-gp)

;;;
;;; statistics
;;;

(defclass stats ()
  ((iteration
    :initarg :iteration :initform nil
    :accessor iteration
    :documentation "Generation/evaluation/etc of the current stats.")))


;;;
;;; common case: fitness
;;;

(defclass fitness-stats (stats)
  ((run-best-raw-score
    :initarg :run-best-raw-score :initform nil
    :accessor run-best-raw-score
    :documentation "Raw score of the best individual.")
   (run-best-fitness-score
    :initarg :run-best-fitness-score :initform nil
    :accessor run-best-fitness-score
    :documentation "Fitness score of the best individual.")
   (best-raw-score
    :initarg :best-raw-score :initform nil
    :accessor best-raw-score
    :documentation "Raw score of the current iteration best individual.")
   (best-fitness-score
    :initarg :best-fitness-score :initform nil
    :accessor best-fitness-score
    :documentation "Fitness score of the current iteration best individual.")
   (worst-raw-score
    :initarg :worst-raw-score :initform nil
    :accessor worst-raw-score
    :documentation "Raw score of the current iteration worst individual.")
   (worst-fitness-score
    :initarg :worst-fitness-score :initform nil
    :accessor worst-fitness-score
    :documentation "Fitness score of the current iteration worst individual.")
   (mean-raw-score
    :initarg :mean-raw-score :initform nil
    :accessor mean-raw-score
    :documentation "Raw score mean of the population.")
   (median-raw-score
    :initarg :median-raw-score :initform nil
    :accessor median-raw-score
    :documentation "Raw score median of the population.")
   (deviation-raw-score
    :initarg :deviation-raw-score :initform nil
    :accessor deviation-raw-score
    :documentation "Standard deviation raw score of the population.")
   (mean-fitness-score
    :initarg :mean-fitness-score :initform nil
    :accessor mean-fitness-score
    :documentation "Fitness score mean of the population.")
   (median-fitness-score
    :initarg :media-fitness-score :initform nil
    :accessor median-fitness-score
    :documentation "Fitness score median of the population.")
   (deviation-fitness-score
    :initarg :deviation-fitness-score :initform nil
    :accessor deviation-fitness-score
    :documentation "Standard deviation fitness score of the population.")))


;;
;; methods

(defgeneric compute-stats (stats iteration population run-best new-best-p comparator)
  (:documentation "Compute the sstatistics for a given iteration."))

(defmethod compute-stats ((stats fitness-stats) iteration population 
			  run-best new-best-p comparator)
  "Compute for the given iteration the fitness stats."
  (let* ((raw-mean 0) (raw-median 0) (raw-deviation 0)
	 (fit-mean 0) (fit-median 0) (fit-deviation 0) (best nil)
	 (size (size population)) (worst nil) (middle (/ size 2))
	 (odd-middle (round middle)) (even-middle (1- middle)))
    (setf (individuals population) 
	  (stable-sort (individuals population) 
		       comparator
		       :key #'(lambda (individual)
				(fitness-score (fitness individual)))))
    (setf best (copy (aref (individuals population) 0)))
    (setf worst (copy (aref (individuals population) (1- size))))
    (loop for individual across (individuals population)
       sum (raw-score (fitness individual)) into raw-total
       sum (fitness-score (fitness individual)) into fit-total
       finally (setf raw-mean (/ raw-total size)
		     fit-mean (/ fit-total size)))
    (loop for individual across (individuals population)
       sum (expt (- (raw-score (fitness individual)) raw-mean) 2) into raw-total
       sum (expt (- (fitness-score (fitness individual)) fit-mean) 2) into fit-total
       finally (setf raw-deviation (sqrt (/ raw-total size))
		     fit-deviation (sqrt (/ fit-total size))))
    (if (oddp size)
	(setf raw-median (raw-score (fitness (aref (individuals population) odd-middle)))
	      fit-median (fitness-score (fitness (aref (individuals population) odd-middle))))
	(setf raw-median (/ (+ (raw-score (fitness 
					   (aref (individuals population) middle)))
			       (raw-score (fitness 
					   (aref (individuals population) even-middle)))) 
			    2)
	      fit-median (/ (+ (fitness-score (fitness 
					       (aref (individuals population) middle)))
			       (fitness-score (fitness 
					       (aref (individuals population) even-middle))))
			    2)))
    (when (funcall comparator 
		   (fitness-score (fitness best))
		   (fitness-score (fitness run-best)))
      (setf run-best (copy best))
      (setf new-best-p t))
    (setf (iteration stats) iteration
	  (run-best-raw-score stats) (float (raw-score (fitness run-best)))
	  (run-best-fitness-score stats) (float (fitness-score (fitness run-best)))
	  (best-raw-score stats) (float (raw-score (fitness best)))
	  (best-fitness-score stats) (float (fitness-score (fitness best)))
	  (worst-raw-score stats) (float (raw-score (fitness worst)))
	  (worst-fitness-score stats) (float (fitness-score (fitness worst)))
	  (mean-raw-score stats) (float raw-mean)
	  (median-raw-score stats) (float raw-median)
	  (deviation-raw-score stats) (float raw-deviation)
	  (mean-fitness-score stats) (float fit-mean)
	  (median-fitness-score stats) (float fit-median)
	  (deviation-fitness-score stats) (float fit-deviation))
    (values run-best new-best-p)))


;;;
;;; structural stats
;;;

(defclass tree-stats (fitness-stats)
  ((mean-depth
    :initarg :mean-depth :initform nil
    :accessor mean-depth
    :documentation "Population's mean tree depth.")
   (deviation-depth
    :initarg :deviation-depth :initform nil
    :accessor deviation-depth
    :documentation "Population's standard deviation tree depth.")
   (mean-nodes-count
    :initarg :mean-nodes-count :initform nil
    :accessor mean-nodes-count
    :documentation "Population's mean tree nodes count.")
   (deviation-nodes-count
    :initarg :deviation-nodes-count :initform nil
    :accessor deviation-nodes-count
    :documentation "Population's standard deviation tree nodes count.")
   (run-best-depth
    :initarg :run-best-depth :initform nil
    :accessor run-best-depth
    :documentation "The tree depth of the run-best individual.")
   (run-best-nodes-count
    :initarg :run-best-nodes-count :initform nil
    :accessor run-best-nodes-count
    :documentation "The tree nodes count of the run-best individual.")
   (best-depth
    :initarg :best-depth :initform nil
    :accessor best-depth
    :documentation "The tree depth of the current iteration best individual.")
   (best-nodes-count
    :initarg :best-nodes-count :initform nil
    :accessor best-nodes-count
    :documentation "The tree nodes count of the current iteration best individual.")
   (worst-depth
    :initarg :worst-depth :initform nil
    :accessor worst-depth
    :documentation "The tree depth of the current iteration worst individual.")
   (worst-nodes-count
    :initarg :worst-nodes-count :initform nil
    :accessor worst-nodes-count
    :documentation "The tree nodes count of the current iteration worst individual.")
   ))

;;
;; methods

(defmethod compute-stats ((stats tree-stats) iteration population 
			  run-best new-best-p comparator)
  "Compute for the given iteration the fitness+stree stats."
  (multiple-value-bind (run-best new-best-p)
      (call-next-method)
    (let* ((size (size population)) 
	   (depth-mean 0) (nodes-mean 0)
	   (depth-deviation 0) (nodes-deviation 0)
	   (best (aref (individuals population) 0)) 
	   (worst (aref (individuals population) (1- size))))
       (loop for individual across (individuals population)
	  sum (tree-depth (genome individual)) into depth-total
	  sum (nodes-count (genome individual)) into nodes-total
	  finally (setf depth-mean (/ depth-total size)
			nodes-mean (/ nodes-total size)))
       (loop for individual across (individuals population)
	  sum (expt (- (tree-depth (genome individual)) depth-mean) 2) into depth-total
	  sum (expt (- (nodes-count (genome individual)) nodes-mean) 2) into nodes-total
	  finally (setf depth-deviation (sqrt (/ depth-total size))
			nodes-deviation (sqrt (/ nodes-total size))))
       (setf (mean-depth stats) (float depth-mean)
	     (deviation-depth stats) (float depth-deviation)
	     (mean-nodes-count stats) (float nodes-mean)
	     (deviation-nodes-count stats) (float nodes-deviation)
	     (run-best-depth stats) (float (tree-depth (genome run-best)))
	     (run-best-nodes-count stats) (float (nodes-count (genome run-best)))
	     (best-depth stats) (float (tree-depth (genome best)))
	     (best-nodes-count stats) (float (nodes-count (genome best)))
	     (worst-depth stats) (float (tree-depth (genome worst)))
	     (worst-nodes-count stats) (float (nodes-count (genome worst))))
       (values run-best new-best-p))))


;;;
;;; algorithm stats
;;;