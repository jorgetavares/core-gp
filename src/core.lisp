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


;;;
;;; genetic operators
;;;

;;;
;;; subtree crossover
;;;

(defun apply-crossover (population size max-depth rate)
  "Apply tree crossover to the population."
  (loop for position from 0 below size by 2
     do (when (< (random 1.0) rate)
	  (multiple-value-bind (o1 o2)
	      (tree-crossover max-depth 
			      (aref population position) 
			      (aref population (1+ position)))
	    (setf (aref population position) o1 
		  (aref population (1+ position)) o2)))))

(defun tree-crossover (size p1 p2)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (individual-tree p1) (individual-tree p2) size)
    (values (make-individual :tree (copy-tree o1) :eval-p t)
	    (make-individual :tree (copy-tree o2) :eval-p t))))

(defun cross-subtrees (p1 p2 depth)
  "Exchanges two subtrees in a random point."
  (let* ((p1-point (random (count-tree-nodes p1)))
         (p2-point (random (count-tree-nodes p2)))
         (o1 (list (copy-tree p1)))
         (o2 (list (copy-tree p2))))
    (multiple-value-bind (p1-subtree p1-fragment)
        (get-subtree (first o1) o1 p1-point)
      (multiple-value-bind
            (p2-subtree p2-fragment)
          (get-subtree
           (first o2) o2 p2-point)
        (setf (first p1-subtree) p2-fragment)
        (setf (first p2-subtree) p1-fragment)))
    (validate-crossover p1 o1 p2 o2 depth)))

(defun get-subtree (tree point index)
  "Return a subtree."
  (if (= index 0)
      (values point (copy-tree tree) index)
      (if (consp tree)
	  (do* ((tree-rest (rest tree) (rest tree-rest))
		(arg (first tree-rest) (first tree-rest)))
	       ((not tree-rest) (values nil nil index))
	    (multiple-value-bind
		  (new-point new-tree new-index)
		(get-subtree arg tree-rest (1- index))
	      (if (= new-index 0)
		  (return (values new-point new-tree new-index))
		  (setf index new-index))))
	  (values nil nil index))))

(defun validate-crossover (p1 o1 p2 o2 depth)
  "Validates the offspring. If they pass the maximum depth they are rejected."
  (let ((p1-limit (tree-depth (first o1)))
        (p2-limit (tree-depth (first o2))))
    (values
     (if (or (= 1 p1-limit) (> p1-limit depth))
         p1 (first o1))
     (if (or (= 1 p2-limit) (> p2-limit depth))
         p2 (first o2)))))

(defun count-tree-nodes (tree)
  "Count the number of nodes in a tree."
  (if (consp tree)
      (+ 1 (reduce #'+ (mapcar #'count-tree-nodes (rest tree)))) 1))
  
(defun tree-depth (tree)
 "Return the max depth of a tree."
 (if (consp tree)
     (+ 1 (if (rest tree)
	      (apply #'max (mapcar #'tree-depth (rest tree))) 0)) 1))

;;;
;;; point mutation
;;;

(defun apply-mutation (population size mt-rate node-rate fset tset tset-size)
  "Apply point mutation crossover to the population."
  (loop for position from 0 below size
     do (when (< (random 1.0) mt-rate)
	  (let ((individual (aref population position)))
	    (point-mutation individual node-rate fset tset tset-size)
	    (setf (individual-eval-p individual) t)))))
	 

(defun point-mutation (individual rate fset tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (individual-tree individual)
	(point-mutate-tree (individual-tree individual) rate fset tset tset-size))
  individual)

(defun point-mutate-tree (tree rate fset tset tset-size)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (if (or (not (consp tree))
	  (null (rest tree)))
      (mutate-terminal tree rate tset tset-size)
      (let* ((element (first tree))
	     (nargs (function-args (assoc element fset))))
	(cons (mutate-function element nargs rate fset) 
	      (loop for arg from 1 to nargs
		 collect (point-mutate-tree (nth arg tree) rate fset tset tset-size))))))

(defun mutate-terminal (terminal rate tset tset-size)
  (if (< (random 1.0) rate)
      (process-terminal (nth (random tset-size) tset))
      terminal))

(defun mutate-function (function nargs rate fset)
  (if (< (random 1.0) rate)
      (let ((filtered-fset (mapcan #'(lambda (f)
				       (when (= nargs (function-args f)) 
					 (list f))) fset)))
	(if (null filtered-fset)
	    function
	    (function-name (nth (random (length filtered-fset)) filtered-fset))))	
      function))


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

