(in-package #:core-gp)

;;;
;;; id generation 
;;;

(defparameter *current-id* 0)

(defun generate-id ()
  "Produce a new id for an individual."
  (incf *current-id*))

(defun reset-id ()
  "Set id to zero."
  (setf *current-id* 0))


;;;
;;; core engine
;;;

(defun run-core (config output streams)
  (let ((population-config (population config))
	(evaluation-config (evaluation config))
	(selection-config (selection config))
	(terminal-config (terminal-condition config))
	(extra-config (extra-configurations config)))
    (let* ((genome-type (genome-type population-config))
	   (replacement-mode (select-replacement-mode 
			      (replacement selection-config)))
   	   (sets (sets extra-config))
	   (comparator (comparator (extra-configurations config)))
	   (inverse-comparator (inverse-comparator 
				(extra-configurations config)))
	   (population nil) 
	   (best nil) (run-best nil) (new-best-p t)
	   (total-generations (if (eql (terminal-condition terminal-config) 
				       :generations)
				  (condition-value terminal-config)
				  (/ (condition-value terminal-config) 
				     (size population-config))))
	   (args (case genome-type
		   (tree-genome 
		    (list (initial-size population-config) 
			  (tree-generator population-config)
			  (functions sets) (functions-size sets)
			  (terminals sets) (terminals-size sets)))
		   (bit-genome
		    (list (genome-size population-config)))
		   (otherwise (error "run-core: no valid genome-type.")))))
      (reset-id)
      (setf population 
	    (apply #'make-random-population (size population-config) genome-type args)) 
      (evaluate-population population evaluation-config)
      (setf best (copy (aref (individuals population) (find-best population comparator))))
      (setf run-best (copy best))
      (output-generation 1 population best run-best new-best-p output streams)
      (loop for generation from 2 to total-generations
	 do (progn
	      (setf new-best-p nil)
	      (funcall replacement-mode population config)
	      (when (elitism-mode selection-config)
		(elitism population best inverse-comparator))
	      (setf best (copy (aref (individuals population)
				     (find-best population comparator))))
	      (when (< (raw-score (fitness best))
		       (raw-score (fitness run-best)))
		(setf run-best (copy best))
		(setf new-best-p t))
	      (output-generation generation population best 
				 run-best new-best-p output streams))
	 finally (return run-best)))))


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
	 (new-population 
	  (loop 
	     with new-individuals = (make-array size)
	     for i from 0 below size
	     do (setf (aref new-individuals i)
		      (funcall (selection-operator (selection config)) 
			       population (comparator (extra-configurations config))))
	     finally (return (make-population :individuals new-individuals :size size)))))
    (apply-crossover new-population config)
    (apply-mutation new-population config)
    (evaluate-population new-population (evaluation config))
    (setf population new-population)))
  
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
			(funcall (mt-operator (operators config)) offspring config))
		  (setf (eval-p offspring) nil)))
	    (evaluate-individual offspring (evaluation config))
	    (setf (aref (individuals population) 
			(index-tournament population 2 (inverse-comparator 
							(extra-configurations config))))
		  (copy offspring))))))

;; GA generic start function
(defun ga-generic (&key (pop-size) (genome-type 'bit-genome) (genome-size 10)
		   (evaluation-fn nil) (scaling-fn nil)
		   (cx-operator #'one-point-crossover) (cx-rate 0.75)
		   (mt-operator #'flip-mutation) (mt-rate 1.0) (mt-gene-rate 0.01)
		   (selection '(tournament 3)) (replacement-mode :generational)
		   (terminal-condition :generations) (terminal-value 10) 
		   (elitism t) (stop nil) (optimum-solution nil)
		   (id "ga") (output :screen) (comparator #'<))
  "Configure and start a GA engine."
  (let ((ga-config (make-core-config
		    (make-linear-population-config pop-size genome-type genome-size)
		    (make-operators-config :cx-operator cx-operator
					   :cx-rate cx-rate
					   :mt-operator mt-operator
					   :mt-rate mt-rate
					   :mt-gene-rate mt-gene-rate)
		    (make-evaluation-config evaluation-fn scaling-fn)
		    (make-selection-config (apply #'make-selection
						  (first selection) (rest selection))
					   replacement-mode elitism)
		    (make-terminal-config terminal-condition 
					  terminal-value stop 
					  optimum-solution)
		    (make-extra-config :comparator comparator))))
    (open-output-streams ga-config output id)))

;; GP generic start function
(defun gp-generic (&key (pop-size) (fset-names nil) (tset-names nil)
		   (size-type :depth) (initial-size 2) (maximum-size 5)
		   (tree-generator #'ramped-half-and-half) 
		   (evaluation-fn nil) (scaling-fn nil)
		   (cx-operator #'tree-crossover) (cx-rate 0.9)
		   (mt-operator #'point-mutation) (mt-rate 0.1) (mt-gene-rate 0.01)
		   (selection '(tournament 3)) (replacement-mode :steady-state)
		   (terminal-condition :generations) (terminal-value 10) 
		   (elitism nil) (stop nil) (optimum-solution nil)
		   (id "gp") (output :screen) (comparator #'<))
  "Configure and start a GP engine."
  (let ((gp-config (make-core-config
		    (make-tree-population-config pop-size size-type initial-size maximum-size tree-generator)
		    (make-operators-config :cx-operator cx-operator
					   :cx-rate cx-rate
					   :mt-operator mt-operator
					   :mt-rate mt-rate
					   :mt-gene-rate mt-gene-rate)
		    (make-evaluation-config evaluation-fn scaling-fn)
		    (make-selection-config (apply #'make-selection
						  (first selection) (rest selection))
					   replacement-mode elitism)
		    (make-terminal-config terminal-condition 
					  terminal-value stop 
					  optimum-solution)
		    (make-extra-config :comparator comparator
				       :sets (make-sets-container fset-names tset-names)))))
    (open-output-streams gp-config output id)))

(defun open-output-streams (config output id)
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream  (concatenate 'string id "-stats.txt")
				   :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best.txt") 
				     :direction :output :if-exists :supersede)
	  (run-core config output (list run-stream best-stream))))
      (run-core config output nil)))


