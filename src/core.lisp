(in-package #:core-gp)

;;;
;;; GP engine
;;;

(defstruct gp-params
  (genome-type 'tree-genome)
  (total-generations 100)
  (pop-size 100)
  (initial-depth 2)
  (max-depth 5)
  (fset nil)
  (tset nil)
  (fitness nil)
  (t-size 3)
  (cx-rate 0.9)
  (mt-rate 0.05)
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

(defun launch-gp (run fset tset &key (id "gp") (output :screen) 
		  (generations 10) (pop-size 10) (initial-depth 2) 
		  (max-depth 5) (elitism t)
		  (fitness-function nil) (params nil) (type :generational)) 
  "Start GP."
  (let* ((fitness fitness-function)
	 (gp-params (if params params
			(make-gp-params :total-generations generations
					:pop-size pop-size
					:initial-depth initial-depth
					:max-depth max-depth
					:fset fset
					:tset tset
					:fitness fitness
					:elitism elitism
					:type type))))
    (config-gp-output gp-params output run id (gp-params-type gp-params))))

(defun config-gp-output (parameters output run id type)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream (concatenate 'string id "-run"
					       (format nil "~D" run) ".txt")
				  :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best" 
						  (format nil "~D" run) ".txt")
				     :direction :output :if-exists :supersede)
	  (run-core parameters output (list run-stream best-stream))))
      (run-core parameters output nil)))

;;;
;;; main evolutionary loop
;;;

(defun run-core (parameters output streams)
  "Main loop."
  (let* ((genome-type (gp-params-genome-type parameters))
	 (type (gp-params-type gp-params)))
	 (replacement-mode (select-replacement-mode type))
	 (total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (tset-size (length tset))
	 (fset-names (gp-params-fset-names parameters))
	 (arity-table (process-fset-arity fset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (population nil)
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t)
	 (args (when (eql genome-type 'tree-genome) 
		 (list #'ramped-half-and-half fset (length fset) tset (length tset))))
	 (config (make-evaluation-config fitness)))
    ;; create and evaluate initial population with new IDs
    (reset-id)
    (setf population 
	  (apply #'make-random-population pop-size genome-type initial-depth args)) 
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
	    (funcall replacement-mode population t-size max-depth cx-rate
		     0.1 mt-rate fset tset tset-size arity-table config)
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

(defun generational (population t-size max-depth cx-rate mt-rate node-rate 
		     fset tset tset-size arity-table config)
  "Generational evolutionary iteration."
  (let ((new-population (selection population t-size #'<)))
    (apply-crossover new-population max-depth cx-rate)
    (apply-mutation new-population mt-rate node-rate 
		    fset tset tset-size arity-table)
    (evaluate-population new-population config)
    (setf population new-population)))
  
(defun steady-state (population t-size max-depth cx-rate mt-rate node-rate 
		     fset tset tset-size arity-table config)
  "Steady state evolutionary iteration."
  (loop for i from 1 to (size population) 
     do (let ((offspring nil))
	  (if (< (random 1.0) cx-rate)
	      (let* ((parent1 (tournament t-size population #'<))
		     (parent2 (tournament t-size population #'<)))
		(setf offspring (tree-crossover max-depth parent1 parent2)))
	      (progn
		(setf offspring (tournament t-size population #'<))
		(setf (genome offspring)
		      (point-mutation offspring mt-rate fset tset tset-size))
		(setf (eval-p offspring) nil)))
	  (evaluate-individual offspring config)
	  (setf (aref (individuals population) (index-tournament t-size population #'>))
		(copy offspring)))))
 

