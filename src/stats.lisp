
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


(defmethod output-stats ((stats fitness-stats) run-best new-best-p output streams)
  "Outputs the computed stats according to the type of output."
  (unless (eql output :none)
    (when (member output '(:screen :screen+files))
      (format t "generation ~a~%raw: ~a ~a ~a ~a ~a ~a~%fit: ~a ~a ~a ~a ~a ~a~%" 
	      (iteration stats) 
	      (run-best-raw-score stats) (best-raw-score stats) (worst-raw-score stats)
	      (mean-raw-score stats) (median-raw-score stats) (deviation-raw-score stats)
	      (run-best-fitness-score stats) (best-fitness-score stats) 
	      (worst-fitness-score stats) (mean-fitness-score stats) 
	      (median-fitness-score stats) (deviation-fitness-score stats)))
    (when (member output '(:files :screen+files))
      (format (first streams) "generation ~a~%raw: ~a ~a ~a ~a ~a ~a~%fit: ~a ~a ~a ~a ~a ~a~%" 
	      (iteration stats) 
	      (run-best-raw-score stats) (best-raw-score stats) (worst-raw-score stats)
	      (mean-raw-score stats) (median-raw-score stats) (deviation-raw-score stats)
	      (run-best-fitness-score stats) (best-fitness-score stats) 
	      (worst-fitness-score stats) (mean-fitness-score stats) 
	      (median-fitness-score stats) (deviation-fitness-score stats))
      (when new-best-p
	(format (second streams) "~a ~%" (list (iteration stats) run-best))))))


;;;
;;; structural stats
;;;



;;;
;;; algorithm stats
;;;