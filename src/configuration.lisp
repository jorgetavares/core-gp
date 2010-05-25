;(in-package #:core-gp)

;;;
;;; Engine configuration 
;;;

;;
;; base configuration

(defclass core-config ()
  ((population
    :initarg :population
    :initform (error "core-config: must provide a population configuration.")
    :reader population
    :documentation "Population configuration (size, genome-type).")
   (operators
    :initarg :operators
    :initform (error "core-config: must provide operators configuration.")
    :reader operators
    :documentation "Genetic operators configuration (crossover, mutation).")
   (evaluation
    :initarg :evaluation
    :initform (error "core-config: must provide an evaluation configuration.")
    :reader evaluation
    :documentation "Evaluation configuration.")
   (selection
    :initarg :selection
    :initform (error "core-config: must provide a selection configuration.")
    :reader selection
    :documentation "Selection configuration.")
   (terminal-condition
    :initarg :terminal-condition
    :initform (error "core-config: must provide a terminal-condition configuration.")
    :reader terminal-condition
    :documentation "Terminal condition configuration.")   
   ))

;;
;; population configuration

(defclass population-config ()
  ((size
    :initarg :size 
    :initform (error "population-config: must provide a population size.")
    :accessor size
    :documentation "Number of individuals in a single population.")
   (genome-type 
    :initarg genome=type
    :initform (error "population-config: must provide a genome type.")
    :reader genome-type
    :documentation "The genome type of the individuals.")))

;;
;; genetic operators

(defclass operators-config ()
  ((crossover-operator
    :initarg :cx-operator 
    :initform (error "operators-config: must provide a crossover operator.")
    :reader cx-operator
    :documentation "Crossover operator.")
   (crossover-rate
    :initarg :cx-rate
    :initform (error "operators-config: must provide a crossover rate.")
    :reader cx-rate
    :documentation "Crossover aplication rate.")
   (mutation-operator
    :initarg :mt-operator
    :initform (error "operators-config: must provide a mutation operator.")
    :reader mt-operator
    :documentation "Mutation operator.")
   (mutation-rate
    :initarg :mt-rate
    :initform (error "operators-config: must provide a mutation aplication rate.")
    :reader mt-rate
    :documentation "Mutation aplication rate.")
   (mutation-gene-rate
    :initarg :mt-gene-rate
    :initform (error "operators-config: must provide a mutation gene rate.")
    :reader mt-gene-rate
    :documentation "Mutation aplication rate, gene by gene.")))

;;
;; selection configuration

(defclass selection-config ()
  ((selection-operator 
    :initarg :selection-operator
    :initform (error "selection-function: must provide a selection operator.")
    :reader selection-operator
    :documentation "Selection operator.")
   (replacement-mode
    :initarg :replacement-mode
    :initform (error "selection-function: must provide a replacement mode.")
    :reader replacement
    :documentation "Replacement mode.")
   (elitism-mode
    :initarg :elitism-mode
    :initform nil
    :reader elitism-mode
    :documentation "Elitism mode.")))

;;
;; terminal configuration

(defclass terminal-config ()
  ((terminal-condition
    :initarg :terminal-condition
    :initform (error "terminal-config: must provide a terminal condition.")
    :reader terminal-condition
    :documentation "Terminal condition: how the algorithm stops.")
   (condition-value
    :initarg :condition-value
    :initform (error "terminal-config: must provide a value for the terminal condition.")
    :reader condition-value
    :documentation "The value for teh condition, e.g., generations, fitness evaluations.")
   (stop-with-optimum-p
    :initarg :stop-with-optimum
    :initform nil
    :reader stop-with-optimum-p
    :documentation "Terminate if the optimum is found (must provide the solution).")
   (optimum-value 
    :reader optimum
    :documentation "The optimum value/solution to terminate the search.")))

;;
;; evaluation configuration

(defclass evaluation-config ()
  ((evaluation-function 
    :initarg :evaluation-function 
    :initform (error "evaluation-config: must provide an evaluation function.")
    :reader evaluation-function
    :documentation "Evaluation function (required).")
   (scaling-function 
    :initarg :scaling-function 
    :initform nil
    :reader scaling-function
    :documentation "Scaling function (optional).")
   (scaling-p 
    :reader scaling-p
    :documentation "Indicates if scaling is set.")))

(defmethod initialize-instance :after ((config evaluation-config) &key)
  (if (slot-value config 'scaling-function)
      (setf (slot-value config 'scaling-p) t)
      (setf (slot-value config 'scaling-p) nil)))

(defun make-evaluation-config (evaluation-function &optional scaling-function)
  "Return an evaluation configuration."
  (make-instance 'evaluation-config
		 :evaluation-function evaluation-function
		 :scaling-function scaling-function))
  

;;;
;;;
;;;

(defstruct core-params
  (genome-type 'tree-genome)
  (total-generations 100)
  (pop-size 100)
  (genome-size 10)
  (initial-depth 2)
  (max-depth 5)
  (sets nil)
  (fitness nil)
  (t-size 3)
  (crossover #'tree-crossover)
  (mutation  #'point-mutation)
  (cx-rate 0.9)
  (mt-rate 0.1)
  (node-rate 0.05)
  (elitism t)
  (type :generational)
  )

