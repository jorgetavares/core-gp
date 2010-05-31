(in-package #:core-gp)

;;;
;;; selection
;;;

(defun tournament (pop comparator tournament-size)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((size (size pop))
	 (population (individuals pop))
	 (best (aref population (random size))))
    (loop for n from 1 below tournament-size
       do (let ((current (aref population (random size))))
	    (when (funcall comparator 
			   (raw-score (fitness current)) 
			   (raw-score (fitness best)))
	      (setf best (clone current)))) ; must be clone and not copy
       finally (return best))))

(defun index-tournament (pop tournament-size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((size (size pop))
	 (population (individuals pop))
	 (bindex (random size))
	 (best (aref population bindex)))
    (loop for n from 1 below tournament-size
       do (let ((index (random size)))
	    (when (funcall comparator 
			   (raw-score (fitness (aref population index))) 
			   (raw-score (fitness best)))
	      (setf best (aref population index))
	      (setf bindex index)))
       finally (return bindex))))

(defun make-selection (operator &rest args)
  "Define a tournament selection for all the population."
  #'(lambda (population comparator)
      (apply operator population comparator args)))

;;;
;;; elitism
;;;

(defun find-best (population comparator)
  "Return the indicies of the best or worst individual according to comparator."
  (loop 
     with pop = (individuals population)
     with best = 0
     for i from 1 below (size population) 
     when (funcall comparator 
		   (raw-score (fitness (aref pop i))) 
		   (raw-score (fitness (aref pop best))))
     do (setf best i)
     finally (return best)))

(defun elitism (population best-individual inverse-comparator)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population inverse-comparator)))
    (setf (aref (individuals population) worst-position) 
	  (copy best-individual)) population))

