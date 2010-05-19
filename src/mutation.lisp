;(in-package #:core-gp)

;;;
;;; point mutation
;;;

(defun apply-mutation (population parameters &optional arity)
  "Apply point mutation crossover to the population."
  (let ((mt-rate (core-params-mt-rate parameters)))
    (loop for position from 0 below (size population)
       do (when (< (random 1.0) mt-rate)
	    (let ((individual (aref (individuals population) position)))
	      (setf (genome individual)
		    (funcall (core-params-mutation parameters)
			     (genome individual) parameters arity))
	      (setf (eval-p individual) t))))))


;;;
;;; GA operators
;;;

(defmethod flip-mutation ((genome bit-genome) parameters &optional arity)
  (declare (ignore arity)) ;; TO BE REMOVED after an FSET/TSET object is created
  (let ((gene-rate (core-params-node-rate parameters))
	(chromossome (chromossome genome)))
    (loop for index from 0 below (size genome)
       when (< (random 1.0) node-rate)
       do (let ((gene (aref chromossome index)))
	    (setf (aref chromossome index)
		  (if (= gene 1) 0 1))))
    genome))


;;;
;;; GP operators
;;;
	 
(defmethod point-mutation ((genome tree-genome) parameters arity)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (let ((node-rate (core-params-node-rate parameters))
	(fset (core-params-fset parameters))
	(tset (core-params-tset parameters)))
    (setf (chromossome genome)
	  (point-mutate-tree (chromossome genome) rate fset tset (length tset) arity))
    genome))

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
 

