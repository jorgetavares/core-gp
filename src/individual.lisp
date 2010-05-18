;(in-package #:core-gp)

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

(defgeneric copy (individual)
  (:documentation "Return a new identical object to individual but with a different id."))

(defmethod copy ((individual individual))
  (make-instance 'individual
		 :id (generate-id)
		 :genome (copy (genome individual))
		 :fitness (copy (fitness individual))
		 :eval-p (eval-p individual)))

(defgeneric clone (individual)
  (:documentation "Return a new identical object to individual."))

(defmethod clone ((individual individual))
  (make-instance 'individual
		 :id (id individual)
		 :genome (copy (genome individual))
		 :fitness (copy (fitness individual))
		 :eval-p (eval-p individual)))


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
    :initarg :genome :initform nil
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


;;;
;;; genome builders
;;;

(defun make-empty-genome (genome-type)
  "Return an empty individual with an empty tree genome."
  (make-instance genome-type))

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

;;;
;;; methods
;;;

(defgeneric copy (genome)
  (:documentation "Return a new identical object to genome."))

(defmethod copy ((genome bit-genome))
  (make-instance 'bit-genome
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

(defgeneric copy (population)
  (:documentation "Return a new identical population."))

(defmethod copy ((population population))
  (let* ((size (size population))
	 (copy (make-array size)))
    (loop for index from 0 below size
       do (setf (aref copy index)
		(copy (aref (individuals population) index)))
       finally (return (make-instance 'population 
				      :individuals copy :size size)))))

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
   (make-array size
	       :initial-contents 
	       (loop repeat size
		  collect (apply #'make-random-individual
				 (generate-id) genome-type genome-size args)))))

