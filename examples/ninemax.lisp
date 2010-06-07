;;;;
;;;; core-gp example: onemax
;;;;

(in-package #:core-gp-examples)

;(declaim (optimize (speed 3) (debug 0) (safety 1)))

;;;
;;; fitness function
;;;

(defun sum-nines (candidate-solution)
  "Sum all bits in the vector. Optimal solution equals all ones."
  (loop for number across candidate-solution sum number))

(defun linear-scaling (max-size)
  "Scales the raw fitness between 0 and 1."
  #'(lambda (raw-fitness)
      (/ raw-fitness max-size)))


;;;
;;; run GP
;;;

(defun ninemax (&key (id "ga-onemax") (output :screen) (pop-size 80) (genome-size 10) 
	       (generations 20))
  (core-gp:ga-generic :id id
		      :output output
		      :pop-size pop-size
		      :genome-type 'core-gp::integer-genome
		      :genome-size genome-size
		      :evaluation-fn #'sum-nines
		      :scaling-fn (linear-scaling (* genome-size 9))
		      :elitism t
		      :replacement-mode :generational
		      :cx-operator #'one-point-crossover
		      :cx-rate 0.75
		      :mt-operator #'swap-mutation
		      :mt-rate 0.1
		      :mt-gene-rate 0.0
		      :terminal-value generations
		      :comparator #'>))

