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

(defun bound-random (min max)
  "Return a random number between min and max."
  (+ min (random (1+ (- max min)))))

(defun make-random-permutation (sequence size)
  "Generate a random permutation from an ordered sequence."
  (loop for i from 0 below size
     do (let* ((position (truncate (+ i (* (- size i) (random 1.0)))))
	       (value (aref sequence position))) 
	  (setf (aref sequence position) (aref sequence i))
	  (setf (aref sequence i) value))
     finally (return sequence)))

