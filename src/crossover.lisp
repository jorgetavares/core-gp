(in-package #:core-gp)

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

