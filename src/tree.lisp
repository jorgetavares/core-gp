(in-package #:core-gp)

;;;
;;; tree utilities
;;;

(defun count-tree-nodes (tree)
  "Count the number of nodes in a tree."
  (if (consp tree)
      (1+ (reduce #'+ (mapcar #'count-tree-nodes (rest tree)))) 1))
  
(defun max-tree-depth (tree)
 "Return the max depth of a tree."
 (if (consp tree)
     (1+ (if (rest tree)
	     (apply #'max (mapcar #'max-tree-depth (rest tree))) 0)) 1))

;;;
;;; tree generators and related functions
;;;

(defun ramped-half-and-half (size limit fset fset-size tset tset-size)
  "A gp tree is created with half of probability for each method."
  (if (< (random 1.0) 0.5)
      (full-method-depth size limit fset fset-size tset tset-size)
      (grow-method-depth size limit fset fset-size tset tset-size)))

(defun full-method-depth (size limit fset fset-size tset tset-size)
  "Random tree using the Full method and depth as size."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let ((function (nth (random fset-size) fset)))
	(cons (operator function)
	      (loop repeat (arity function) 
		 collect (full-method-depth 
			  (1+ size) limit fset fset-size tset tset-size))))))

(defun grow-method-depth (size limit fset fset-size tset tset-size)
  "Random tree suing the Grow method and depth as size."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let ((index (random (+ fset-size tset-size))))
	(if (< index fset-size) 
	    (let ((function (nth index fset)))
	      (cons (operator function)
		    (loop repeat (arity function)
		       collect (grow-method-depth
				(1+ size) limit fset fset-size tset tset-size))))
	    (process-terminal (nth (- index fset-size) tset))))))
	     
(defun process-terminal (terminal)
  "Process the type of terminal."
  (if (ephemeral terminal)
      (funcall (operator terminal))
      (list (operator terminal))))
