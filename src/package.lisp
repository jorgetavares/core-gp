(defpackage #:core-gp
  (:use #:common-lisp)
  (:export #:core-config ;; configurations.lisp
	   #:population
	   #:operators
	   #:evaluation
	   #:selection
	   #:terminal-condition
	   #:extra-configurations
	   #:make-core-config
	   #:extra-config
	   #:comparator
	   #:inverse-comparator
	   #:sets
	   #:make-extra-config
	   #:population-config
	   #:size
	   #:genome-type
	   #:genome-size
	   #:make-linear-population-config
	   #:tree-population-config
	   #:tree-size-type
	   #:initial-size
	   #:maximum-size
	   #:tree-generator
	   #:make-tree-population-config
	   #:operators-config
	   #:cx-operator
	   #:cx-rate
	   #:mt-operator
	   #:mt-rate
	   #:mt-gene-rate
	   #:make-operators-config
	   #:selection-config
	   #:selection-operator
	   #:replacement-mode
	   #:elitism-mode
	   #:make-selection-config
	   #:terminal-config
	   #:terminal-condition
	   #:condition-value
	   #:stop-with-optimum
	   #:optimum-solution
	   #:make-terminal-config
	   #:evaluation-config
	   #:evaluation-function
	   #:scaling-function
	   #:scaling-p
	   #:make-evaluation-config
	   #:*current-id* ;; core.lisp
	   #:generate-id
	   #:reset-id
	   #:ga-generic
	   #:gp-generic
	   #:open-output-streams
	   #:run-core
	   #:select-replacement-mode
	   #:generational
	   #:steady-state
	   #:apply-crossover ;; crossover.lisp
	   #:one-point-crossover
	   #:tree-crossover
	   #:fitness ;; evaluation.lisp
	   #:raw-score
	   #:fitness-score
	   #:make-fitness
	   #:copy
	   #:set-fitness
	   #:evaluate-genome
	   #:evaluate-individual
	   #:evaluate-population
	   #:individual ;; individual.lisp
	   #:id
	   #:genome
	   #:fitness
	   #:eval-p
	   #:clone
	   #:make-random-individual
	   #:genome
	   #:chromossome
	   #:tree-genome
	   #:tree-depth
	   #:nodes-count
	   #:linear-genome
	   #:size
	   #:bit-genome
	   #:make-empty-genome
	   #:make-bit-genome
	   #:make-tree-genome
	   #:make-random-genome
	   #:population
	   #:individuals
	   #:make-population
	   #:make-random-population
	   #:apply-mutation ;; mutation.lisp
	   #:flip-mutation
	   #:point-mutation
	   #:output-generation ;; output.lisp
	   #:tournament ;; selection.lisp
	   #:index-tournament
	   #:make-selection
	   #:find-best
	   #:elitism
	   #:count-tree-nodes ;; tree.lisp
	   #:max-tree-depth
	   #:ramped-half-and-half
	   #:full-method-depth
	   #:grow-method-depth
	   #:copy-array ;; utilities.lisp
	   #:average-fitness
	   #:sets-container ;; sets.lisp
	   #:functions
	   #:terminals
	   #:functions-size
	   #:terminals-size
	   #:make-sets-container
	   #:make-set
	   #:process-fset-arity
	   #:random-terminal-node
	   #:random-function-node
	   #:find-terminal-node
	   #:find-function-nodes
	   #:defnode
	   #:gp-plus
	   #:gp-minus
	   #:gp-times
	   #:gp-division
	   #:gp-log
	   #:gp-square-root
	   #:gp-square
	   #:gp-power
	   #:gp-if
	   #:gp-and
	   #:gp-or
	   #:gp-not
	   #:gp-<
	   #:gp-<=
	   #:gp->
	   #:gp->=
	   #:gp-=
	   #:gp-/=
	   #:gp-random-n
	   #:*generate-constant*
	   #:gp-constant
	   #:gp-constant-int
	   #:gp-constant-real
	   #:gp-true
	   #:gp-false
	   #:gp-random-real
	   #:gp-random-10
	   #:gp-random-100
	   ))
  