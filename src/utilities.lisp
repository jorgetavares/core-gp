(in-package #:core-gp)

;;;
;;; utilities
;;;

(defun copy-array (array)
  (let ((dimensions (array-dimensions array)))
    (adjust-array
     (make-array dimensions 
		 :element-type (array-element-type array) 
		 :displaced-to array) 
     dimensions)))

(defun average-fitness (population)
  "Average of population's fitness."
  (loop for individual across (individuals population)
     sum (raw-score (fitness individual)) into avg-raw
     if (numberp (raw-score (fitness individual))) 
     sum (raw-score (fitness individual)) into avg-fit
     else sum 0 into avg-fit
     finally (return (values 
		      (/ avg-raw (size population))
		      (if (> avg-fit 0)
			  (/ avg-fit (size population))
			  0)))))
