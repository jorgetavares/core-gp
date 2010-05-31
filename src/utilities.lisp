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
     finally (return (values 
		      (/ avg-raw (size population))
		      0))))

