;;;;
;;;; simple example of STGP
;;;;

(defpackage #:core-gp-stgp-example
  (:use #:common-lisp #:core-gp)
  (:export #:stgp-example
	   #:stgp-var-x
	   #:stgp-a
	   #:stgp-b
	   #:stgp-c
	   #:stgp-d
	   #:int-constants
	   #:make-fitness-stgp
	   #:*X*
	   #:*node-types*
	   #:*fset*
	   #:*tset*
	   ))

(in-package #:core-gp-stgp-example)

;;;
;;; function and terminal sets
;;;

;; terminals
(defstterm stgp-a (:return-type :none 
		   :ephemeral nil
		   :string "A")
  'A)

(defstterm stgp-b (:return-type :none
		   :ephemeral nil
		   :string "B")
  'B)

(defstterm stgp-c (:return-type :none 
		   :ephemeral nil
		   :string "C")
  'C)

(defstterm stgp-d (:return-type :none 
		   :ephemeral nil
		   :string "D")
  'D)

;; variable
(defparameter *X* 0)
(defstterm stgp-var-x (:return-type :none
		       :ephemeral nil
		       :string "X") 
  *X*)

;; constants
(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))

(defstnode stgp-eql ((x y) (:return-type :boolean
			     :args-type '(:any :any)
			     :string "eql")) 
  (eql x y))

(defparameter *fset* '(stgp-if stgp-and stgp-or 
		       stgp-less stgp-greater stgp-equal 
		       stgp-plus stgp-minus))
(defparameter *tset* '(stgp-a stgp-b stgp-c stgp-d 
		       stgp-constant-ints stgp-constant-booleans))


;;;
;;; fitness function
;;;

(defparameter *points* '((1 A) (2 A) (3 A)
			 (4 B) (5 B) 
			 (6 C) (7 C) (8 C) (9 C)
			 (10 D)))

;; '( 2 + 1 )

(defun filter-tree (tree)
  (if (atom tree)
      (core-gp::value tree)
      (if (and (consp tree)
	       (null tree))
	  tree
	  (cons (first tree)
		(loop for arg in (rest tree)
		   collect (filter-tree arg))))))



(defun make-fitness-stgp ()
  #'(lambda (candidate-solution)
      (loop for i from 1 to 10
	 do (setf *X* i)
	 sum (let ((result (eval (filter-tree candidate-solution))))
	       (cond ((and (< i 4)
			   (eql result 'A)) 1)
		     ((and (> i 3) (< i 6)
			   (eql result 'B)) 1)
		     ((and (> i 5) (< i 10)
			   (eql result 'C)) 1)
		     ((and (> i 9) 
			   (eql result 'D)) 1)
		     (t 0))))))

;;;
;;; run GP
;;;

(defun stgp-example (&key (id "stgp-example") (output :screen) (pop-size 1000) (generations 20))
  (setf core-gp:*generate-constant* (int-constants 1 10))
  (core-gp:stgp-generic :id id
			:output output
			:pop-size pop-size
			:fset-names *fset*
			:tset-names *tset*
			:types-tree *stgp-node-types*
			:initial-size 2
			:maximum-size 15
			:mutation-limit 5
			:tree-generator #'ramped-half-and-half-st
			:evaluation-fn (make-fitness-stgp)
			:elitism nil
			:replacement-mode :generational
			:terminal-value generations
			:comparator #'>))
		      
;(defparameter *sets* (make-sets-container-st *fset* *tset* *stgp-node-types*))
;(full-method-depth-st 0 4 (functions *sets*) 8 (terminals *sets*) 6 (functions-types-table *sets*) (terminals-types-table *sets*) nil)









