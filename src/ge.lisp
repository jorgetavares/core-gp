(in-package :core-gp)




;;;
;;; grammar utils
;;;

(defparameter *grammar* nil)

(defun set-grammar (grammar)
  (setf *grammar* grammar) t)


;; (name -> productions)

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun random-mod (choices value)
  (elt choices (mod value (length choices))))

(Defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;;;
;;; mapping functions
;;;

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))
  

(defun make-map-ge (mapping grammar wrap)
  (setf *grammar* grammar)
  (let ((start-symbol (rule-lhs (first *grammar*))))
    #'(lambda (genome)
	(funcall mapping start-symbol genome wrap))))

(defun run-map-ge (genome &key (grammar *grammar*) (wrap t))
  (setf *grammar* grammar)
  (map-ge (rule-lhs (first grammar)) genome wrap))

(defun convert-to-list (chromossome)
  (loop for gene across chromossome collect gene))

(defun map-ge (phrase genome wrap)
  (let ((genome-copy (convert-to-list genome))
	(end (- (length genome) 2)))
    (labels ((generate-ge (grammar)
	       (cond ((listp grammar)
		      (mapcar #'generate-ge grammar))
		     ((rewrites grammar)
		      (generate-ge (random-mod 
				    (rewrites grammar) 
				    (if wrap 
					(let ((value (pop genome-copy)))
					  (push value (cdr (nthcdr end genome-copy)))
					  value)
					(pop genome-copy)))))
		     (t grammar)))) 
      (generate-ge phrase))))

;; extra 
(defun map-tree-ge (phrase genome wrap)
  (let ((genome-copy (copy-list genome))
	(end (- (length genome) 2)))
    (labels ((generate-ge (grammar)
	       (cond ((listp grammar)
		      (mapcar #'generate-ge grammar))
		     ((rewrites grammar)
		      (cons grammar 
			    (generate-ge (random-mod 
					  (rewrites grammar) 
					  (if wrap 
					      (let ((value (pop genome-copy)))
						(format t "~a " value)
						(push value (cdr (nthcdr end genome-copy)))
						value)
					      (let ((value (pop genome-copy)))
						(format t "~a " value) value))))))
		      (t (list grammar))))) 
      (generate-ge phrase))))

(defun random-map-ge (phrase)
  (labels ((generate-ge (grammar)
	     (cond ((listp grammar)
		    (mapcar #'generate-ge grammar))
		   ((rewrites grammar)
		    (generate-ge (random-elt (rewrites grammar))))
		   (t grammar)))) 
    (generate-ge phrase)))

;;;
;;; genome
;;;

(defclass ge-genome (integer-genome)
  ())

(defun make-ge-genome (chromossome size)
  (make-instance 'ge-genome
		 :chromossome chromossome :size size))

(defmethod make-random-genome ((new-genome ge-genome) size &rest args)
  (destructuring-bind (min max) args
    (setf (chromossome new-genome)
	  (make-array size
		      :element-type 'fixnum
		      :initial-contents (loop repeat size 
					      collect (bound-random min max)))
	  (size new-genome) size) new-genome))

(defmethod copy ((genome ge-genome))
  (make-instance 'ge-genome
		 :chromossome (copy-array (chromossome genome))
		 :size (size genome)))

(defmethod print-object ((object ge-genome) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (chromossome size) object ; WARNING: only prints with wrap! 
      (format stream "~a~% code: ~a~% size: ~a" chromossome 
	      (when chromossome
		(run-map-ge chromossome)) size)))) 

;;;
;;; operators
;;;


(defmethod one-point-crossover ((genome1 ge-genome) (genome2 ge-genome) config)
  (let ((size (genome-size (population config))))
      (multiple-value-bind (o1 o2)
	  (cross-chromossomes (chromossome genome1) (chromossome genome2) size)
	(values (make-instance
		 'individual 
		 :id (generate-id) :genome (make-ge-genome o1 size)
		 :fitness (make-fitness (fitness-type (evaluation config))))
		(make-instance 
		 'individual 
		 :id (generate-id) :genome (make-ge-genome o2 size)
		 :fitness (make-fitness (fitness-type (evaluation config))))))))

 
(defmethod uniform-crossover ((genome1 ge-genome) (genome2 ge-genome) config)
  (let ((size (genome-size (population config))))
    (multiple-value-bind (o1 o2)
	(uniform-cross-chromossomes (chromossome genome1) (chromossome genome2) size)
      (values (make-instance
	       'individual 
	       :id (generate-id) :genome (make-ge-genome o1 size)
	       :fitness (make-fitness (fitness-type (evaluation config))))
	      (make-instance 
	       'individual 
	       :id (generate-id) :genome (make-ge-genome o2 size)
	       :fitness (make-fitness (fitness-type (evaluation config))))))))

(defmethod flip-mutation ((genome ge-genome) config)
  (let ((gene-rate (mt-gene-rate (operators config)))
	(chromossome (chromossome genome))
	(min (lower-bound (extra-configurations config)))
	(max (upper-bound (extra-configurations config))))
    (loop for index from 0 below (size genome)
       when (< (random 1.0) gene-rate)
       do (setf (aref chromossome index)
		(bound-random min max)))
    genome))

(defmethod swap-mutation ((genome ge-genome) config)
  (let ((chromossome (chromossome genome))
	(gene1 (random (size genome)))
	(gene2 (random (size genome))))
    (let ((allele1 (aref chromossome gene1)))
      (setf (aref chromossome gene1) (aref chromossome gene2)
	    (aref chromossome gene2) allele1))
    genome))

