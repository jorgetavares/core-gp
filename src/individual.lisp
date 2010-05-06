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
    :initarg :genome
    :initform nil
    :accessor genome
    :documentation "Individual's genome (tree, permutation, etc).")
   (fitness
    :initarg :fitness
    :initform nil
    :accessor fitness
    :documentation "Individual's fitness value.")
   (eval-p
    :initarg :eval-p
    :initform t
    :accessor eval-p
    :documentation "Predicate which indicates if individual needs to be evaluated.")
   ))


;;;
;;; genome definitions
;;;

(defclass genome ()
  ((genome
    :initarg :genome
    :initform nil
    :accessor genome
    :documentation "Genome slot.")
   ))

(defclass tree-genome (genome)
   ((tree-depth
    :init-arg :tree-depth
    :initform 0
    :accessor tree-depth
    :documentation "The individuals's tree depth.")
   (nodes-count
    :init-arg :nodes-count
    :initform 0
    :accessor nodes-count
    :documentation "The amount of function and terminal nodes in the tree.")
    ))

(defclass linear-genome (genome)
  ((size
    :initarg :size
    :initform 0
    :accessor size
    :documentation "Genome's current size.")
   ))


;;;
;;; genome builders
;;;

(defun make-empty-tree-genome ()
  (make-instance 'tree-genome))
  
(defun make-random-tree-genome ()
  
  )



;;;
;;; individual builders
;;;

(defun make-tree-individual ()

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct individual
  (tree nil)
  (fitness 0)
  (info nil)  ; extra stuff about the individual 
  (eval-p t)) ; if eval-p is t, it means that tree must be evaluated

(defun safe-copy-individual (individual)
  "Fresh copy of a individual structure."
  (make-individual
   :tree (copy-tree (individual-tree individual))
   :fitness (individual-fitness individual)))

(defun make-random-individual (tree-limit fset tset)
  "Return a random generate tree without being evaluated."
  (make-individual :tree (ramped-half-and-half tree-limit fset tset)))

(defun make-population (size tree-limit fset tset)
  "Return an array filled with random gp individuals."
  (make-array size 
	      :initial-contents (loop repeat size 
				   collect (make-random-individual tree-limit fset tset))))
