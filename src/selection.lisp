(in-package #:core-gp)

;;;
;;; selection
;;;

(defun tournament (tournament-size population comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((size (size population))
	 (population (individuals population))
	 (best (aref (individuals population) (random size))))
    (loop for n from 1 below tournament-size
       do (let ((current (aref population (random size))))
	    (when (funcall comparator 
			   (raw-score (fitness current)) 
			   (raw-score (fitness best)))
	      (setf best (clone current)))) ; must be clone and not copy
       finally (return best))))

(defun index-tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((size (size population))
	 (population (individuals population))
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

(defun selection (population tournament-size comparator)
  "Return a new population."
  (let ((size (size population)))
    (loop 
       with new-individuals = (make-array size)
       for i from 0 below size
       do (setf (aref new-individuals i)
		(tournament tournament-size population comparator))
       finally (return (make-population new-individuals size)))))


;;;
;;; elitism
;;;

(defun find-best (population comparator)
  "Return the indicies of the best or worst individual according to comparator."
  (loop 
     with population = (individuals population)
     with best = 0
     for i from 1 below (size population) 
     when (funcall comparator 
		   (raw-score (fitness (aref population i))) 
		   (raw-score fitness (aref population best)))
     do (setf best i)
     finally (return best)))

(defun elitism (population best-individual)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population #'>)))
    (setf (aref (individuals population) worst-position) 
	  (copy best-individual)) population))

