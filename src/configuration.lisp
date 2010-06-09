(in-package #:core-gp)

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
   (extra-configurations
    :initarg :extra
    :initform (make-instance 'extra-config)
    :reader extra-configurations
    :documentation "Extra elements.")
   ))

(defun make-core-config (population operators evaluation selection terminal extra)
  "Return an algorithm configuration."
  (make-instance 'core-config
		 :population population
		 :operators operators
		 :evaluation evaluation
		 :selection selection
		 :terminal-condition terminal
		 :extra extra))

;;
;; extra configurations

(defclass extra-config ()
  ((comparator
    :initarg :comparator :initform #'<
    :reader comparator
    :documentation "Defines if the algorithm minimizes or maximizes.")
   (inverse-comparator
    :reader inverse-comparator
    :documentation "The opposite of the comparator.")
   (stats-type
    :initarg :stats-type :initform 'fitness-stats
    :reader stats-type
    :documentation "The type of statistics to collect.")
   (sets 
    :initarg :sets :initform nil
    :reader sets 
    :documentation "Function and Terminal Sets for GP algorithms.")
   (lower-bound
    :initarg :lower-bound :initform 0
    :accessor lower-bound
    :documentation "Minimum value for a gene (allele).")
   (upper-bound
    :initarg :upper-bound :initform 9
    :accessor upper-bound
    :documentation "Maximum value for a gene (allele).")))

(defmethod initialize-instance :after ((config extra-config) &key)
  (if (eql (slot-value config 'comparator) #'<)
      (setf (slot-value config 'inverse-comparator) #'>)
      (setf (slot-value config 'inverse-comparator) #'<)))

(defun make-extra-config (&key comparator stats-type sets lower-bound upper-bound)
  "Return a configuration of extra elements."
  (make-instance 'extra-config 
		 :comparator comparator
		 :stats-type stats-type
		 :sets sets
		 :lower-bound lower-bound
		 :upper-bound upper-bound))

;;
;; population configuration

(defclass population-config ()
  ((size
    :initarg :size 
    :initform (error "population-config: must provide a population size.")
    :accessor size
    :documentation "Number of individuals in a single population.")
   (genome-type 
    :initarg :genome-type
    :initform (error "population-config: must provide a genome type.")
    :reader genome-type
    :documentation "The genome type of the individuals.")))

;; for populations with linear genomes
(defclass linear-population-config (population-config)
  ((genome-size
    :initarg :genome-size
    :initform (error "linear-population-config: must provide a genome length.")
    :reader genome-size
    :documentation "Length of the individual's genome.")))

(defun make-linear-population-config (size genome-type genome-size)
  "Return a population configuration."
  (make-instance 'linear-population-config
		 :size size
		 :genome-type genome-type
		 :genome-size genome-size))

;; for populations with tree genomes
(defclass tree-population-config (population-config)
  ((size-type
    :initarg :tree-size-type :initform :depth
    :reader tree-size-type
    :documentation "The type of the tree size :depth or :node-count.")
   (initial-size
    :initarg :initial-size :initform 0
    :reader initial-size
    :documentation "Initial tree depth or number of nodes.")
   (maximum-size
    :initarg :maximum-size :initform 0
    :reader maximum-size
    :documentation "Maximum tree depth or number of nodes.")
   (tree-generator
    :initarg :tree-generator
    :initform #'ramped-half-and-half
    :reader tree-generator
    :documentation "Tree generator.")))

(defun make-tree-population-config (pop-size tree-size-type initial maximum generator)
  "Return a tree population configuration."
  (make-instance 'tree-population-config
		 :size pop-size
		 :genome-type 'tree-genome
		 :tree-size-type tree-size-type
		 :initial-size initial
		 :maximum-size maximum
		 :tree-generator generator))

;;
;; genetic operators

(defclass operators-config ()
  ((crossover-operator
    :initarg :cx-operator 
    :initform nil
    :reader cx-operator
    :documentation "Crossover operator.")
   (crossover-rate
    :initarg :cx-rate
    :initform nil
    :reader cx-rate
    :documentation "Crossover aplication rate.")
   (mutation-operator
    :initarg :mt-operator
    :initform nil
    :reader mt-operator
    :documentation "Mutation operator.")
   (mutation-rate
    :initarg :mt-rate
    :initform nil
    :reader mt-rate
    :documentation "Mutation aplication rate.")
   (mutation-gene-rate
    :initarg :mt-gene-rate
    :initform nil
    :reader mt-gene-rate
    :documentation "Mutation aplication rate, gene by gene.")))

(defun make-operators-config (&key cx-operator cx-rate mt-operator mt-rate mt-gene-rate)
  "Return a genetic operators configuration."
  (make-instance 'operators-config
		 :cx-operator cx-operator
		 :cx-rate cx-rate
		 :mt-operator mt-operator
		 :mt-rate mt-rate
		 :mt-gene-rate mt-gene-rate))

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

(defun make-selection-config (selection replacement &optional elitism)
  "Return a selection configuration"
  (make-instance 'selection-config
		 :selection-operator selection
		 :replacement-mode replacement
		 :elitism-mode elitism))

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
   (stop-with-optimum
    :initarg :stop-with-optimum
    :initform nil
    :reader stop-with-optimum-p
    :documentation "Indicates if search should be stoped when optimum is found.")
   (optimum-solution
    :initarg :optimum-solution
    :initform nil
    :reader optimum-solution
    :documentation "Solution to be compared to stop the search.")))

(defun make-terminal-config (condition value &optional stop solution)
  "Return a temrinal condition configuration."
  (make-instance 'terminal-config
		 :terminal-condition condition
		 :condition-value value
		 :stop-with-optimum stop
		 :optimum-solution solution))

;;
;; evaluation configuration

(defclass evaluation-config ()
  ((fitness-type
    :initarg :fitness-type
    :initform (error "evaluation-config: must provide a fitness type.")
    :reader fitness-type
    :documentation "The type of fitness (should default to 'fitness).")
   (evaluation-function 
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

(defun make-evaluation-config (fitness-type evaluation-function &optional scaling-function)
  "Return an evaluation configuration."
  (make-instance 'evaluation-config
		 :fitness-type fitness-type
		 :evaluation-function evaluation-function
		 :scaling-function scaling-function))
