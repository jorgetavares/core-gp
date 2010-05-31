(in-package #:core-gp)

;;;
;;; output
;;;

(defun output-generation (generation population best run-best new-best-p output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-raw (raw-score (fitness best))))
      (multiple-value-bind (avg-raw avg-fit)
	  (average-fitness population)
	(progn
	  (when (member output '(:screen :screen+files))
	    (format t "~a ~a ~a ~a ~a~%" generation
		    (float best-raw) (float avg-raw) nil nil)) 
	  (when (member output '(:files :screen+files))
	    (format (first streams) "~a ~a ~a ~a ~a~%" generation
		    (float best-raw) (float avg-raw) nil nil)
	    (when new-best-p
	      (format (second streams) "~a ~%" (list generation run-best)))))))))
  
