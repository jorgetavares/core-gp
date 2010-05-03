(in-package #:core-gp)

;;;;
;;;; GP engine 
;;;;

;;;
;;; representation and population initialization
;;;

(defstruct individual
  (tree nil)
  (fitness 0)
  (info nil)  ; extra stuff about the individual 
  (eval-p t)) ; if eval-p is t, it means that tree must be evaluated

(defun safe-copy-individual (individual)
  "Fresh copy of a individual structure."
  (make-individual
   :tree (copy-tree (individual-tree individual))
   :fitness (individual-fitness individual)))

(defun make-random-individual (tree-limit fset tset)
  "Return a random generate tree without being evaluated."
  (make-individual :tree (ramped-half-and-half tree-limit fset tset)))

(defun make-population (size tree-limit fset tset)
  "Return an array filled with random gp individuals."
  (make-array size 
	      :initial-contents (loop repeat size 
				   collect (make-random-individual tree-limit fset tset))))

;;;
;;; evaluations
;;;

(defun eval-population (population size fitness-function generation)
  "Set the fitness to every element in the population."
  (loop 
     for individual across population
     for id from 1 to size
     do (eval-individual individual fitness-function id generation)))

(defun eval-individual (individual fitness-function id generation)
  "Set the fitness function of a single individual."
  (when (individual-eval-p individual)
    (setf (individual-fitness individual) 
	  (funcall fitness-function individual id generation))
    (setf (individual-eval-p individual) nil)))


;;;
;;; GP engine
;;;

(defstruct gp-params
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

(defun launch-gp (fset tset &key (id "gp") (runs 1) (output :screen) 
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
    (gp-multiple-runs gp-params :runs runs :output output :id id 
		      :type (gp-params-type gp-params))))

(defun launch-gp2 (run fset tset &key (id "gp") (output :screen) 
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

(defun gp-multiple-runs (parameters &key (runs 1) (output :screen) (id "gp") (type :generational))
  "Run the gp engine for several runs."
  (loop for run from 1 to runs
     do (format t "GP run ~a~%" run)
     collect (time (config-gp-output parameters output run id type))))

(defun config-gp-output (parameters output run id type)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream (concatenate 'string id "-run" (format nil "~D" run) ".txt")
				  :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best" (format nil "~D" run) ".txt")
				     :direction :output :if-exists :supersede)
	  (funcall (run-gp-type type) parameters output (list run-stream best-stream))))
      (funcall (run-gp-type type) parameters output nil)))

(defun run-gp-type (type)
  "Return the appropriate generational model to launch gp."
  (case type
    (:generational #'run-single-gp)
    (:steady-state #'run-steady-state)
    (otherwise (error "Invalid generational model for GP engine."))))

(defun run-single-gp (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (tset-size (length tset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness 1)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    (loop for generation from 2 to total-generations
       do (let ((new-population (selection population pop-size t-size #'<)))
	    (setf new-best-p nil)
	    (apply-crossover new-population pop-size max-depth cx-rate)
	    (apply-mutation new-population pop-size 0.1 mt-rate fset tset tset-size)
	    (eval-population new-population pop-size fitness generation)
	    (when elitism-p
	      (elitism new-population pop-size best))
	    (setf population new-population)
	    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	    (when (< (individual-fitness best) (individual-fitness run-best))
	      (setf run-best (copy-individual best))
	      (setf new-best-p t))
	    (output-generation generation population pop-size best 
			       run-best new-best-p output streams))
       finally (return run-best))))

(defun run-steady-state (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (tset-size (length tset))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (mt-rate (gp-params-mt-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil) (new-best-p t))
    (eval-population population pop-size fitness 1)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best new-best-p output streams)
    ;(dump-trees run-best population pop-size 1 3)
    (loop for generation from 2 to total-generations
       do (progn
	    (setf new-best-p nil)
	    (loop for i from 1 to pop-size 
	       do (let ((offspring nil))
		    (if (< (random 1.0) cx-rate)
			(let* ((parent1 (tournament t-size population pop-size #'<))
			       (parent2 (tournament t-size population pop-size #'<)))
			  (setf offspring (tree-crossover max-depth parent1 parent2)))
			(progn 
			  (setf offspring
				(point-mutation 
				 (tournament t-size population pop-size #'<) 
				 mt-rate fset tset tset-size))
			  (setf (individual-eval-p offspring) nil)))
		    (eval-individual offspring fitness i generation)
		    (setf (aref population (index-tournament t-size population pop-size #'>))
			  (copy-individual offspring))))
	    (when elitism-p
	      (elitism population pop-size best))
	    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	    (when (< (individual-fitness best) (individual-fitness run-best))
	      (setf run-best (copy-individual best))
	      (setf new-best-p t))
	    ;(when (member generation '(5 10 20 25))
	    ;  (dump-trees run-best population pop-size generation 3))
	    (output-generation generation population pop-size best 
			       run-best new-best-p output streams))
       finally (return run-best))))

(defun dump-trees (best population pop-size generation n-random)
  (with-open-file (out-dump (concatenate 'string "dump-trees-" 
					 (format nil "~D" generation)
					 ".txt")
			    :direction :output :if-exists :supersede)
    (format out-dump "~a~%~%" best)
    (loop for i from 1 to n-random
       do (format out-dump "~a~%" (aref population (random pop-size))))))


(defun output-generation (generation population pop-size best run-best 
			  new-best-p output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-fitness (float (individual-fitness best)))
	  (avg (float (average population pop-size))))
      (when (member output '(:screen :screen+files))
	(format t "~a ~a ~a ~%" generation best-fitness avg))
      (when (member output '(:files :screen+files))
	(format (first streams) "~a ~a ~a ~%" generation best-fitness avg)
	(when new-best-p
	  (format (second streams) "~a ~%" (list generation run-best)))))))
  

(defun average (population pop-size)
  "Average of population's fitness."
  (loop for individual across population
     sum (individual-fitness individual) into total
     finally (return (/ total pop-size))))

