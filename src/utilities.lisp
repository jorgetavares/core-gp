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


