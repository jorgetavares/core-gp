(in-package #:core-gp)

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

