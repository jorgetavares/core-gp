(in-package #:core-gp)

;;;
;;; selection
;;;

(defun tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let ((best (aref population (random size))))
    (loop for n from 1 below tournament-size
       do (let ((current (aref population (random size))))
	    (when (funcall comparator (individual-fitness current) (individual-fitness best))
	      (setf best (copy-individual current))))
       finally (return best))))

(defun index-tournament (tournament-size population size comparator)
  "Tournament selection: return best individual from a random set of a given size."
  (let* ((bindex (random size))
	 (best (aref population bindex)))
    (loop for n from 1 below tournament-size
       do (let ((index (random size)))
	    (when (funcall comparator (individual-fitness (aref population index)) 
			   (individual-fitness best))
	      (setf best (aref population index))
	      (setf bindex index)))
       finally (return bindex))))

(defun selection (population size tournament-size comparator)
  "Return a new population."
  (loop with new-population = (make-array size)
     for i from 0 below size
     do (setf (aref new-population i)
	      (tournament tournament-size population size comparator))
     finally (return new-population)))


;;;
;;; elitism
;;;

(defun find-best (population size comparator)
  "Return the indicies of the best or worst individuals in the population, according to comparator."
  (loop 
     with best = 0
     for i from 1 below size 
     when (funcall comparator 
		   (individual-fitness (aref population i)) 
		   (individual-fitness (aref population best)))
     do (setf best i)
     finally (return best)))

(defun elitism (population size best-individual)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population size #'>)))
    (setf (aref population worst-position) 
	  (copy-individual best-individual)) population))

