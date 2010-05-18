;(in-package #:core-gp)

;;;
;;; point mutation
;;;

(defun apply-mutation (population mt-rate node-rate fset tset tset-size arity)
  "Apply point mutation crossover to the population."
  (loop for position from 0 below (size population)
     do (when (< (random 1.0) mt-rate)
	  (let ((individual (aref (individuals population) position)))
	    (setf (genome individual)
		  (point-mutation (genome individual) node-rate fset tset tset-size arity))
	    (setf (eval-p individual) t)))))
	 
(defmethod point-mutation ((genome tree-genome) node-rate fset tset tset-size arity)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (chromossome genome)
	(point-mutate-tree (chromossome genome) rate fset tset tset-size arity))
  genome)

(defun point-mutate-tree (tree rate fset tset tset-size arity)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (if (or (not (consp tree))
	  (null (rest tree)))
      (mutate-terminal tree rate tset tset-size)
      (let* ((element (first tree))
	     (nargs (arity (find element fset #:test #'(lambda (name node)
							 (eql name (operator node)))))))
	(cons (mutate-function element nargs rate arity) 
	      (loop for arg from 1 to nargs
		 collect (point-mutate-tree (nth arg tree) rate 
					    fset tset tset-size arity))))))

(defun mutate-terminal (terminal rate tset tset-size)
  (if (< (random 1.0) rate)
      (process-terminal (nth (random tset-size) tset))
      terminal))

(defun mutate-function (function nargs rate arity)
  (if (< (random 1.0) rate)
      (let ((same-args (gethash nargs arity)))
	(operator (nth (random (length same-args)) same-args)))
      function))
 

