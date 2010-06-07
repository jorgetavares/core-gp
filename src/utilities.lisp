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


