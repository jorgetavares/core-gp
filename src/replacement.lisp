(in-package #:core-gp)

;;;
;;; replacement modes
;;;

(defun select-replacement-mode (type)
  "Return the appropriate generational model to launch gp."
  (case type
    (:generational #'generational)
    (:steady-state #'steady-state)
    (otherwise (error "Invalid generational model for core-gp engine."))))

(defun generational (population config)
  "Generational evolutionary iteration."
  (let* ((size (size population))
	 (new-individuals (make-array size)))
    (loop 
       for i from 0 below size
       do (setf (aref new-individuals i)
		(funcall (selection-operator (selection config)) 
			 population (comparator (extra-configurations config))))
       finally (setf (individuals population) new-individuals))
    (apply-crossover population config)
    (apply-mutation population config)
    (evaluate-population population (evaluation config))))
  
(defun steady-state (population config)
  "Steady state evolutionary iteration."
  (let ((selection-fn (selection-operator (selection config)))
	(comparator (comparator (extra-configurations config))))
    (loop 
       for i from 1 to (size population) 
       do (let ((offspring nil))
	    (if (< (random 1.0) (cx-rate (operators config)))
		(let* ((parent1 (funcall selection-fn population comparator))
		       (parent2 (funcall selection-fn population comparator)))
		  (setf offspring 
			(funcall (cx-operator (operators config)) 
				 (genome parent1) (genome parent2) config)))
		(progn
		  (setf offspring (funcall selection-fn population comparator))
		  (setf (genome offspring)
			(funcall (mt-operator (operators config)) (genome offspring) config))
		  (setf (eval-p offspring) nil)))
	    (evaluate-individual offspring (evaluation config))
	    (setf (aref (individuals population) 
			(index-tournament population 2 (inverse-comparator 
							(extra-configurations config))))
		  (copy offspring))))))

