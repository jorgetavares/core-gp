(in-package #:core-gp)

;;;
;;; subtree crossover
;;;

(defun apply-crossover (pop config)
  "Apply tree crossover to the population."
  (loop with population = (individuals pop) 
     for position from 0 below (size pop) by 2
     do (when (< (random 1.0) (cx-rate (operators config)))
	  (multiple-value-bind (o1 o2)
	      (funcall (cx-operator (operators config))
		       (genome (aref population position)) 
		       (genome (aref population (1+ position))) config)
	    (setf (aref population position) o1 
		  (aref population (1+ position)) o2)))))


;;;
;;; GA operators
;;;

(defgeneric one-point-crossover (genome1 genome2 config)
  (:documentation "One point crossover operator."))

(defmethod one-point-crossover ((genome1 bit-genome) (genome2 bit-genome) config)
  (let ((size (genome-size (population config))))
      (multiple-value-bind (o1 o2)
	  (cross-bit-chromossomes (chromossome genome1) (chromossome genome2) size)
	(values (make-instance
		 'individual 
		 :id (generate-id) :genome (make-bit-genome o1 size))
		(make-instance 
		 'individual 
		 :id (generate-id) :genome (make-bit-genome o2 size))))))

(defun cross-bit-chromossomes (c1 c2 size)
  (let ((cut-point (random size))
	(o1 (copy-array c1))
	(o2 (copy-array c2)))
    (loop for index from cut-point below size
       do (progn
	    (setf (aref o1 index) (aref c2 index))
	    (setf (aref o2 index) (aref c1 index)))
       finally (return (values o1 o2)))))
       

;;;
;;; GP operators
;;;

(defgeneric tree-crossover (genome1 genome2 config)
  (:documentation "Tree crossover operator."))

(defmethod tree-crossover ((genome1 tree-genome) (genome2 tree-genome) config)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (chromossome genome1) 
		      (chromossome genome2) 
		      (maximum-size (population config)))
    (values (make-instance 
	     'individual 
	     :id (generate-id)
	     :genome (make-tree-genome
		      (copy-tree o1) (max-tree-depth o1) (count-tree-nodes o1)))
	    (make-instance 
	     'individual 
	     :id (generate-id)
	     :genome (make-tree-genome
		      (copy-tree o2) (max-tree-depth o2) (count-tree-nodes o2))))))

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
  (let ((p1-limit (max-tree-depth (first o1)))
        (p2-limit (max-tree-depth (first o2))))
    (values
     (if (or (= 1 p1-limit) (> p1-limit depth))
         p1 (first o1))
     (if (or (= 1 p2-limit) (> p2-limit depth))
         p2 (first o2)))))


