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

;; GA generic start function
(defun ga-generic (&key (pop-size) (genome-type 'bit-genome) (genome-size 10)
		   (fitness-type 'fitness) (evaluation-fn nil) (scaling-fn nil)
		   (cx-operator #'one-point-crossover) (cx-rate 0.75)
		   (mt-operator #'flip-mutation) (mt-rate 1.0) (mt-gene-rate 0.01)
		   (selection '(tournament 3)) (replacement-mode :generational)
		   (terminal-condition :generations) (terminal-value 10) 
		   (elitism t) (stop nil) (optimum-solution nil)
		   (id "ga") (output :screen) (comparator #'<) (lower 0) (upper 9))
  "Configure and start a GA engine."
  (let ((ga-config (make-core-config
		    (make-linear-population-config pop-size genome-type genome-size)
		    (make-operators-config :cx-operator cx-operator
					   :cx-rate cx-rate
					   :mt-operator mt-operator
					   :mt-rate mt-rate
					   :mt-gene-rate mt-gene-rate)
		    (make-evaluation-config fitness-type evaluation-fn scaling-fn)
		    (make-selection-config (apply #'make-selection
						  (first selection) (rest selection))
					   replacement-mode elitism)
		    (make-terminal-config terminal-condition 
					  terminal-value stop 
					  optimum-solution)
		    (make-extra-config :comparator comparator 
				       :stats-type 'fitness-stats
				       :lower-bound lower
				       :upper-bound upper))))
    (open-output-streams ga-config output id)))

;; GP generic start function
(defun gp-generic (&key (pop-size) (fset-names nil) (tset-names nil)
		   (size-type :depth) (initial-size 2) (maximum-size 5)
		   (tree-generator #'ramped-half-and-half) 
		   (fitness-type 'fitness) (evaluation-fn nil) (scaling-fn nil)
		   (cx-operator #'tree-crossover) (cx-rate 0.9)
		   (mt-operator #'point-mutation) (mt-rate 0.1) (mt-gene-rate 0.01)
		   (selection '(tournament 3)) (replacement-mode :steady-state)
		   (terminal-condition :generations) (terminal-value 10) 
		   (elitism nil) (stop nil) (optimum-solution nil)
		   (id "gp") (output :screen) (comparator #'<))
  "Configure and start a GP engine."
  (let ((gp-config (make-core-config
		    (make-tree-population-config pop-size size-type 
						 initial-size maximum-size tree-generator)
		    (make-operators-config :cx-operator cx-operator
					   :cx-rate cx-rate
					   :mt-operator mt-operator
					   :mt-rate mt-rate
					   :mt-gene-rate mt-gene-rate)
		    (make-evaluation-config fitness-type evaluation-fn scaling-fn)
		    (make-selection-config (apply #'make-selection
						  (first selection) (rest selection))
					   replacement-mode elitism)
		    (make-terminal-config terminal-condition 
					  terminal-value stop 
					  optimum-solution)
		    (make-extra-config :comparator comparator
				       :sets (make-sets-container fset-names tset-names)
				       :stats-type 'tree-stats))))
    (open-output-streams gp-config output id)))

;; Strong-Type GP generic start function
(defun stgp-generic (&key (pop-size) (fset-names nil) (tset-names nil) (types-tree nil)
		     (size-type :depth) (initial-size 2) (maximum-size 5)
		     (tree-generator #'ramped-half-and-half-st) 
		     (fitness-type 'fitness) (evaluation-fn nil) (scaling-fn nil)
		     (cx-operator #'stgp-tree-crossover) (cx-rate 0.9)
		     (mt-operator #'stgp-tree-mutation) (mt-rate 0.1) (mt-gene-rate 0.1)
		     (selection '(tournament 3)) (replacement-mode :steady-state)
		     (terminal-condition :generations) (terminal-value 10) 
		     (elitism nil) (stop nil) (optimum-solution nil)
		     (id "stgp") (output :screen) (comparator #'<) (mutation-limit 2))
  "Configure and start a STGP engine."
  (let ((gp-config (make-core-config
		    (make-tree-population-config pop-size size-type 
						 initial-size maximum-size tree-generator)
		    (make-operators-config :cx-operator cx-operator
					   :cx-rate cx-rate
					   :mt-operator mt-operator
					   :mt-rate mt-rate
					   :mt-gene-rate mt-gene-rate)
		    (make-evaluation-config fitness-type evaluation-fn scaling-fn)
		    (make-selection-config (apply #'make-selection
						  (first selection) (rest selection))
					   replacement-mode elitism)
		    (make-terminal-config terminal-condition 
					  terminal-value stop 
					  optimum-solution)
		    (make-extra-config :comparator comparator
				       :upper-bound mutation-limit
				       :sets (make-sets-container-st 
					      fset-names tset-names types-tree)
				       :stats-type 'tree-stats))))
    (open-output-streams gp-config output id)))


;;
;; genric core

(defun open-output-streams (config output id)
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream  (concatenate 'string id "-stats.txt")
				   :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best.txt") 
				     :direction :output :if-exists :supersede)
	  (run-core config output (list run-stream best-stream))))
      (run-core config output nil)))

(defun run-core (config output streams)
  (let ((population-config (population config))
	(evaluation-config (evaluation config))
	(selection-config (selection config))
	(terminal-config (terminal-condition config))
	(extra-config (extra-configurations config)))
    (let* ((genome-type (genome-type population-config))
	   (replacement-mode (select-replacement-mode 
			      (replacement selection-config)))
   	   (comparator (comparator (extra-configurations config)))
	   (inverse-comparator (inverse-comparator 
				(extra-configurations config)))
	   (population nil) (run-best nil) (new-best-p t)
	   (total-generations (if (eql (terminal-condition terminal-config) 
				       :generations)
				  (condition-value terminal-config)
				  (/ (condition-value terminal-config) 
				     (size population-config))))
	   (args (case genome-type
		   (tree-genome 
		    (list (initial-size population-config) 
			  (tree-generator population-config)
			  (functions (sets extra-config)) (functions-size (sets extra-config))
			  (terminals (sets extra-config)) (terminals-size (sets extra-config))
			  (functions-types-table (sets extra-config)) 
			  (terminals-types-table (sets extra-config))))
		   (bit-genome
		    (list (genome-size population-config)))
		   (integer-genome
		    (list (genome-size population-config)
			  (lower-bound extra-config)
			  (upper-bound extra-config)))
		   (permutation-genome
		    (list (genome-size population-config)
			  (lower-bound extra-config)
			  (upper-bound extra-config)))
		   (otherwise (error "run-core: no valid genome-type."))))
	   (stats (make-array total-generations 
			      :initial-element (make-instance (stats-type extra-config)))))
      (reset-id)
      (setf population 
	    (apply #'make-random-population 
		    (fitness-type evaluation-config) (size population-config) genome-type args))
      (evaluate-population population evaluation-config)
      (multiple-value-bind (new-run-best flag)
	  (compute-stats (aref stats 0) 1 population (aref (individuals population) 0)
			 new-best-p comparator)
	(setf run-best new-run-best new-best-p flag))
      (output-stats (aref stats 0) run-best new-best-p output streams)
      (loop for generation from 2 to total-generations
	 do (progn
	      (setf new-best-p nil)
	      (funcall replacement-mode population config)
	      (when (elitism-mode selection-config)
		(elitism population run-best inverse-comparator))
	      (multiple-value-bind (new-run-best flag)
		  (compute-stats (aref stats (1- generation)) generation population
				 run-best new-best-p comparator)
		(setf run-best new-run-best new-best-p flag))
	      (output-stats  (aref stats (1- generation)) run-best new-best-p output streams))
	 finally (return run-best)))))
