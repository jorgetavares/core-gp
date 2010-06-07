(in-package #:core-gp)

;;;
;;; representation and population initialization
;;;

;;;
;;; base individual
;;;

(defclass individual ()
  ((id
    :initarg :id
    :initform (error "Class individual: must supply a unique id.") 
    :reader id
    :documentation "Unique ID of the individual.")
   (genome
    :initarg :genome :initform nil
    :accessor genome
    :documentation "Individual's genome (tree, permutation, etc).")
   (fitness
    :initarg :fitness :initform (make-fitness)
    :accessor fitness
    :documentation "Individual's fitness value.")
   (eval-p
    :initarg :eval-p :initform t
    :accessor eval-p
    :documentation "Predicate which indicates if individual needs to be evaluated.")))

;;;
;;; individual methods
;;;

(defgeneric copy (object)
  (:documentation "Returna copy of the object."))

(defgeneric clone (individual)
  (:documentation "Return a new identical object to individual."))


(defmethod copy ((individual individual))
  "Return a new identical object to individual but with a different id."
  (make-instance 'individual
		 :id (generate-id)
		 :genome (copy (genome individual))
		 :fitness (copy (fitness individual))
		 :eval-p (eval-p individual)))

(defmethod clone ((individual individual))
  (make-instance 'individual
		 :id (id individual)
		 :genome (copy (genome individual))
		 :fitness (copy (fitness individual))
		 :eval-p (eval-p individual)))

(defmethod print-object ((object individual) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id genome fitness eval-p) object
      (format stream "id: ~a eval-p: ~a~%~a~%~a" id eval-p genome fitness)))) 


;;;
;;; individual builders
;;;

(defun make-random-individual (id genome-type size &rest args)
  "Return a random individual of a specific defined genome."
  (make-instance 'individual
		 :id id
		 :genome (apply #'make-random-genome 
				(make-empty-genome genome-type) size args)))

;;;
;;; genome definitions
;;;

(defclass genome ()
  ((chromossome
    :initarg :chromossome :initform nil
    :accessor chromossome
    :documentation "Genome slot.")))

(defclass tree-genome (genome)
   ((tree-depth
     :initarg :tree-depth :initform 0
     :accessor tree-depth
     :documentation "The individuals's tree depth.")
    (nodes-count
     :initarg :nodes-count :initform 0
     :accessor nodes-count
     :documentation "The amount of function and terminal nodes in the tree.")))

(defclass linear-genome (genome)
  ((size
    :initarg :size :initform 0
    :accessor size
    :documentation "Genome's current size.")))

(defclass bit-genome (linear-genome)
  ())

(defclass integer-genome (linear-genome)
  ())

(defclass permutation-genome (integer-genome)
  ())


;;;
;;; genome builders
;;;

;;
;; functions

(defun make-empty-genome (genome-type)
  "Return an empty individual with an empty tree genome."
  (make-instance genome-type))

(defun make-bit-genome (chromossome size)
  (make-instance 'bit-genome :chromossome chromossome :size size))

(defun make-tree-genome (tree depth nodes-count)
  (make-instance 'tree-genome 
		 :chromossome tree 
		 :tree-depth depth :nodes-count nodes-count))

(defun make-integer-genome (chromossome size)
  (make-instance 'integer-genome
		 :chromossome chromossome :size size))

(defun make-permutation-genome (chromossome size)
  (make-instance 'permutation-genome
		 :chromossome chromossome :size size))


;;
;; methods
		 
(defgeneric make-random-genome (new-genome size &rest args)
  (:documentation "Generate a random genome of given type and size."))
    
(defmethod make-random-genome ((new-genome tree-genome) size &rest args)
  (destructuring-bind (builder fset fset-size tset tset-size) args
    (let ((tree (funcall builder 0 size fset fset-size tset tset-size)))
      (setf (chromossome new-genome) tree
	    (tree-depth new-genome) (max-tree-depth tree)
	    (nodes-count new-genome) (count-tree-nodes tree))
      new-genome)))

(defmethod make-random-genome ((new-genome bit-genome) size &rest args)
  (declare (ignore args))
  (setf (chromossome new-genome)
	(make-array size 
		    :element-type 'bit
		    :initial-contents (loop repeat size collect (random 2)))
	(size new-genome) size) new-genome)

(defmethod make-random-genome ((new-genome integer-genome) size &rest args)
  (destructuring-bind (min max) args
    (setf (chromossome new-genome)
	  (make-array size
		      :element-type 'fixnum
		      :initial-contents (loop repeat size 
					   collect (bound-random min max)))
	  (size new-genome) size) new-genome))

(defmethod make-random-genome ((new-genome integer-genome) size &rest args)
  (destructuring-bind (min max) args
    (setf (chromossome new-genome)
	  (make-random-permutation
	   (make-array size
		       :element-type 'fixnum
		       :initial-contents (loop for n from min to max collect n)) size)
	  (size new-genome) size) new-genome))

	
;;;
;;; methods
;;;

(defmethod copy ((genome bit-genome))
  (make-instance 'bit-genome
		 :chromossome (copy-array (chromossome genome))
		 :size (size genome)))

(defmethod copy ((genome integer-genome))
  (make-instance 'integer-genome
		 :chromossome (copy-array (chromossome genome))
		 :size (size genome)))

(defmethod copy ((genome permutation-genome))
  (make-instance 'permutation-genome
		 :chromossome (copy-array (chromossome genome))
		 :size (size genome)))

(defmethod copy ((genome linear-genome))
  (make-instance 'linear-genome
		 :chromossome (copy-array (chromossome genome))
		 :size (size genome)))

(defmethod copy ((genome tree-genome))
  (make-instance 'tree-genome
		 :chromossome (copy-tree (chromossome genome))
		 :tree-depth (tree-depth genome)
		 :nodes-count (nodes-count genome)))

(defmethod print-object ((object genome) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (chromossome) object
      (format stream "~a" chromossome)))) 

(defmethod print-object ((object linear-genome) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (chromossome size) object
      (format stream "~a size: ~a" chromossome size)))) 

(defmethod print-object ((object tree-genome) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (chromossome tree-depth nodes-count) object
      (format stream "~a depth: ~a nodes-count: ~a" 
	      chromossome tree-depth nodes-count)))) 

;;;
;;; population
;;;

(defclass population ()
  ((individuals
    :initarg :individuals :initform nil
    :accessor individuals
    :documentation "Population's individuals.")
   (size
    :initarg :size :initform 0
    :accessor size
    :documentation "Number of individuals in the population.")))

;;;
;;; methods
;;;

(defmethod copy ((population population))
  (let* ((size (size population))
	 (copy (make-array size)))
    (loop for index from 0 below size
       do (setf (aref copy index)
		(copy (aref (individuals population) index)))
       finally (return (make-instance 'population 
				      :individuals copy :size size)))))

(defmethod print-object ((object population) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (individuals size) object
      (format stream "~a~% size: ~a" individuals size)))) 


;;;
;;; builders
;;;

(defun make-population (&key individuals size)
  "Return a population instance, empty or filled."
  (make-instance 'population 
		 :individuals individuals
		 :size (if size size (length individuals))))

(defun make-random-population (size genome-type genome-size &rest args)
  (make-population 
   :individuals
   (make-array size
	       :initial-contents 
	       (loop repeat size
		  collect (apply #'make-random-individual
				 (generate-id) genome-type genome-size args)))
   :size size))

