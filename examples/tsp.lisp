;;;;
;;;; core-gp example: tsp
;;;;

(defpackage #:core-gp-tsp
  (:use #:common-lisp #:core-gp #:cl-tsplib)
  (:export #:tsp 
	   #:make-tsp-fitness 
	   #:*tsp-tsp-instance-filename*))

(in-package #:core-gp-tsp)


;;;
;;; fitness function
;;;

(defparameter *tsp-instance-filename* cl-tsplib:*burma14*)

(defun make-tsp-fitness (distances size)
  "Computes the length of a symmetric tsp route for a given instance."
  #'(lambda (tour)
      (+ (loop 
	    for i from 0 below (1- size)
	    for j from 1 below size
	    sum (aref distances (aref tour i) (aref tour j)))
	 (aref distances (aref tour (1- size)) (aref tour 0)))))


;;;
;;; run GA
;;;

(defun tsp (&key (id "ga-tsp") (output :screen) (pop-size 100) (generations 100)
	    (tsp-filename *tsp-instance-filename*))
  (let* ((tsp-instance  (cl-tsplib:parse-problem-instance tsp-filename))
	 (instance-data (cl-tsplib:problem-instance-distance-matrix tsp-instance))
	 (instance-size (cl-tsplib:problem-instance-dimension tsp-instance)))
    (core-gp:ga-generic :id id
			:output output
			:pop-size pop-size
			:genome-type 'permutation-genome
			:genome-size instance-size
			:lower 1
			:upper instance-size
			:evaluation-fn (make-tsp-fitness
					instance-data instance-size)
			:elitism t
			:replacement-mode :generational
			:cx-operator #'uniform-crossover
			:cx-rate 0.75
			:mt-operator #'swap-mutation
			:mt-rate 0.1
			:mt-gene-rate 0.0
			:terminal-value generations
			:comparator #'<)))

