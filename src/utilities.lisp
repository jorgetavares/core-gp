(in-package #:core-gp)

;;;
;;; utilities
;;;

(defun dump-trees (best population pop-size generation n-random)
  (with-open-file (out-dump (concatenate 'string "dump-trees-" 
					 (format nil "~D" generation)
					 ".txt")
			    :direction :output :if-exists :supersede)
    (format out-dump "~a~%~%" best)
    (loop for i from 1 to n-random
       do (format out-dump "~a~%" (aref population (random pop-size))))))

(defun average (population pop-size)
  "Average of population's fitness."
  (loop for individual across population
     sum (individual-fitness individual) into total
     finally (return (/ total pop-size))))