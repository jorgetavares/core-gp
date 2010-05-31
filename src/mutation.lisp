(in-package #:core-gp)

;;;
;;; point mutation
;;;

(defun apply-mutation (population config)
  "Apply point mutation crossover to the population."
  (let ((mt-rate (mt-rate (operators config))))
    (loop for position from 0 below (size population)
       do (when (< (random 1.0) mt-rate)
	    (let ((individual (aref (individuals population) position)))
	      (setf (genome individual)
		    (funcall (mt-operator (operators config))
			     (genome individual) config))
	      (setf (eval-p individual) t))))))


;;;
;;; GA operators
;;;

(defgeneric flip-mutation (genome config)
  (:documentation "Flip mutation operator."))

(defmethod flip-mutation ((genome bit-genome) config)
  (let ((gene-rate (mt-gene-rate (operators config)))
	(chromossome (chromossome genome)))
    (loop for index from 0 below (size genome)
       when (< (random 1.0) gene-rate)
       do (let ((gene (aref chromossome index)))
	    (setf (aref chromossome index)
		  (if (= gene 1) 0 1))))
    genome))


;;;
;;; GP operators
;;;

(defgeneric point-mutation (genome config)
  (:documentation "Point mutation operator."))
	 
(defmethod point-mutation ((genome tree-genome) config)
  "Point mutation: for every node that can be mutated, changes to an equivalent type."
  (setf (chromossome genome)
	(point-mutate-tree (chromossome genome) 
			   (mt-gene-rate (operators config)) 
			   (sets (extra-configurations config))))
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
