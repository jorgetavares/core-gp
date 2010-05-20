(in-package #:core-gp)

;;;
;;; GP engine
;;;

(defstruct core-params
  (genome-type 'tree-genome)
  (total-generations 100)
  (pop-size 100)
  (genome-size 10)
  (initial-depth 2)
  (max-depth 5)
  (sets nil)
  (fitness nil)
  (t-size 3)
  (crossover #'tree-crossover)
  (mutation  #'point-mutation)
  (cx-rate 0.9)
  (mt-rate 0.1)
  (node-rate 0.05)
  (elitism t)
  (type :generational)
  )


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

(defun ga (&key (generations 10) (pop-size) (genome-type 'bit-genome) (genome-size 10)
	   (type :generational) (elitism t) (evaluation-function nil) 
	   (crossover #'one-point-crossover) (mutation #'flip-mutation)
	   (id "ga") (output :screen) (params nil))
  "Start a GA engine."
  (let* ((fitness evaluation-function)
	 (core-params (if params params
			  (make-core-params :genome-type genome-type
					    :total-generations generations
					    :pop-size pop-size
					    :genome-size genome-size
					    :fitness fitness
					    :elitism elitism
					    :crossover crossover
					    :mutation mutation
					    :type type))))
    (config-output core-params output id (core-params-type core-params))))

(defun gp (sets &key (id "gp") (output :screen) 
	   (generations 10) (pop-size 10) (initial-depth 2) 
	   (max-depth 5) (elitism t) (crossover #'tree-crossover) (mutation #'point-mutation)
	   (fitness-function nil) (params nil) (type :generational)) 
  "Start a GP engine."
  (let* ((fitness fitness-function)
	 (core-params (if params params
			  (make-core-params :total-generations generations
					    :pop-size pop-size
					    :initial-depth initial-depth
					    :max-depth max-depth
					    :sets sets
					    :fitness fitness
					    :elitism elitism
					    :crossover crossover
					    :mutation mutation
					    :type type))))
    (config-output core-params output id (core-params-type core-params))))

(defun config-output (parameters output id type)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream  (concatenate 'string "stats-" id)
				   :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string "best-" id) 
				     :direction :output :if-exists :supersede)
	  (run-core parameters output (list run-stream best-stream))))
      (run-core parameters output nil)))


;;;
;;; main evolutionary loop
;;;

(defun run-core (parameters output streams)
  "Main loop."
  (let* ((genome-type (core-params-genome-type parameters))
	 (genome-size (core-params-genome-size parameters))
	 (type (core-params-type parameters)))
	 (replacement-mode (select-replacement-mode type))
	 (total-generations (core-params-total-generations parameters))
	 (initial-depth (core-params-initial-depth parameters))
	 (sets (core-params-sets parameters))
	 (fitness (core-params-fitness parameters))
	 (t-size (core-params-t-size parameters))
	 (cx-rate (core-params-cx-rate parameters))
	 (mt-rate (core-params-mt-rate parameters))
	 (node-rate (core-params-node-rate parameters))
	 (population nil)
	 (elitism-p (core-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t)
	 (args (case genome-type
		 ('tree-genome 
		  (list initial-depth #'ramped-half-and-half 
			(functions sets) (functions-size sets)
			(terminals sets) (terminals-size sets)))
		 ('bit-genome
		  (list genome-size))
		 (otherwise (error "run-core: no valid genome-type."))))
	 (config (make-evaluation-config fitness)))
    ;; create and evaluate initial population with new IDs
    (reset-id)
    (setf population 
	  (apply #'make-random-population (core-params-pop-size parameters) genome-type args)) 
    (evaluate-population population config)
    ;; keep track of best individuals (generation and run)
    (setf best (copy (aref (individuals population) (find-best population #'<))))
    (setf run-best (copy best))
    ;; compute stats for initial population (first generation)
    (output-generation 1 population best run-best new-best-p output streams)
    ;; process the remaining generations
    (loop for generation from 2 to total-generations
       do (progn
	    (setf new-best-p nil)
	    ;; do a generational or steady-state iteration
	    (funcall replacement-mode population parameters t-size cx-rate
		     mt-rate node-rate config)
	    (when elitism-p (elitism population best))
	    ;; get best of the current population
	    (setf best (copy (aref (individuals population)
				   (find-best population #'<))))
	    ;; determine if is a best of run
	    (when (< (raw-score (fitness best))
		     (raw-score (fitness run-best)))
	      (setf run-best (copy best))
	      (setf new-best-p t))
	    ;; output stats
	    (output-generation generation population best 
			       run-best new-best-p output streams))
       finally (return run-best)))


;;;
;;; replacement modes
;;;

(defun select-replacement-mode (type)
  "Return the appropriate generational model to launch gp."
  (case type
    (:generational #'generational)
    (:steady-state #'steady-state)
    (otherwise (error "Invalid generational model for core-gp engine."))))

(defun generational (population parameters t-size cx-rate mt-rate node-rate config)
  "Generational evolutionary iteration."
  (let ((new-population (selection population t-size #'<)))
    (apply-crossover new-population parameters)
    (apply-mutation new-population parameters)
    (evaluate-population new-population config)
    (setf population new-population)))
  
(defun steady-state (population parameters t-size cx-rate mt-rate node-rate config)
  "Steady state evolutionary iteration."
  (loop for i from 1 to (size population) 
     do (let ((offspring nil))
	  (if (< (random 1.0) cx-rate)
	      (let* ((parent1 (tournament t-size population #'<))
		     (parent2 (tournament t-size population #'<)))
		(setf offspring 
		      (funcall (core-params-crossover parameters)
			       (genome parent1) (genome parent2) parameters)))
	      (progn
		(setf offspring (tournament t-size population #'<))
		(setf (genome offspring)
		      (funcall (core-params-mutation parameters)
			       offspring parameters))
		(setf (eval-p offspring) nil)))
	  (evaluate-individual offspring config)
	  (setf (aref (individuals population) (index-tournament t-size population #'>))
		(copy offspring)))))
 

