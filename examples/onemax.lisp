;;;;
;;;; core-gp example: onemax
;;;;

(in-package #:core-gp-examples)


;;;
;;; fitness function
;;;

(defun count-ones (candidate-solution)
  "Sum all bits in the vector. Optimal solution equals all ones."
  (loop for bit across candidate-solution sum bit))


;;;
;;; run GP
;;;

(defun onemax (&key (id "ga-onemax") (output :screen) (pop-size 80) (genome-size 20) (generations 100))
  (core-gp:ga-generic :id id
		      :output output
		      :pop-size pop-size
		      :genome-size genome-size
		      :evaluation-fn #'count-ones
		      :elitism t
		      :replacement-mode :generational
		      :cx-operator #'one-point-crossover
		      :cx-rate 0.75
		      :mt-operator #'flip-mutation
		      :mt-gene-rate 0.01
		      :terminal-value generations
		      :comparator #'>))

