;(in-package #:core-gp)

;;;
;;; point mutation
;;;

(defun apply-mutation (population parameters)
  "Apply point mutation crossover to the population."
  (let ((mt-rate (core-params-mt-rate parameters)))
    (loop for position from 0 below (size population)
       do (when (< (random 1.0) mt-rate)
	    (let ((individual (aref (individuals population) position)))
	      (setf (genome individual)
		    (funcall (core-params-mutation parameters)
			     (genome individual) parameters))
	      (setf (eval-p individual) t))))))


;;;
;;; GA operators
;;;

(defmethod flip-mutation ((genome bit-genome) parameters)
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
	 
(defmethod point-mutation ((genome tree-genome) parameters)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (chromossome genome)
	(point-mutate-tree (chromossome genome) 
			   (core-params-node-rate parameters) 
			   (core-params-sets parameters)))
  genome)

(defun point-mutate-tree (tree rate sets)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (if (or (not (consp tree))
	  (null (rest tree)))
      (mutate-terminal tree rate sets)
      (let* ((node-name (first tree))
	     (node-arity (arity (find-function-node node-name sets))))
	(cons (mutate-function node-name node-arity rate sets) 
	      (loop for arg from 1 to node-arity
		 collect (point-mutate-tree (nth arg tree) rate sets))))))


(defun mutate-terminal (terminal rate sets)
  (if (< (random 1.0) rate)
      (process-terminal (random-terminal-node sets))
      terminal))

(defun mutate-function (function arity rate sets)
  (if (< (random 1.0) rate)
      (operator (random-function-node sets arity))
      function))
