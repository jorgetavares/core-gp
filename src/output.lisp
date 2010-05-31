(in-package #:core-gp)

;;;
;;; output
;;;

(defun output-generation (generation population best run-best new-best-p output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-raw (raw-score (fitness best)))
	  (avg-raw (average-fitness population)))
      (when (member output '(:screen :screen+files))
	(format t "~a ~a ~a~%" generation
		(float best-raw) (float avg-raw))) 
      (when (member output '(:files :screen+files))
	(format (first streams) "~a ~a ~a~%" generation
		(float best-raw) (float avg-raw))
	(when new-best-p
	  (format (second streams) "~a ~%" (list generation run-best)))))))
  