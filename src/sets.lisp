;(in-package #:core-gp)

;;;
;;; sets container
;;;

(defclass sets-container ()
  ((functions
    :initarg :functions 
    :initform (error "Class sets-container: must provide a list with functions.")
    :accessor functions
    :documentation "Function Set.")
   (terminals
    :initarg :terminals 
    :initform (error "Class sets-container: must provide a list with terminals.")
    :accessor terminals
    (:documentation "Terminal Set."))
   (functions-size
    :reader functions-size
    :documentation "Number of elements in the Function Set.")
   (terminals-size
    :reader terminals-size
    :documentation "Number of elements in the Terminal Set.")
   (arity-table
    :reader arity-table
    :documentation "Function Set organized by muber of arguments.")))


;;;
;;; builder

(defmethod initialize-instance :after ((container sets-container) &key)
  (setf (slot-value container 'functions-size) 
	(length (slot-value container 'functions)))
  (setf (slot-value container 'terminals-size)
	(length (slot-value container 'terminals)))
  (setf (slot-value container 'arity-table)
	(process-fset-arity (slot-value container 'functions))))
  
(defun make-sets-container (function-names terminal-names)
  "Return a container for the Function and Terminal Sets (nodes must be defined)."
  (make-instance 'sets-container
		 :functions (make-set function-names)
		 :terminals (make-set terminal-names)))

(defun make-set (node-names)
  "Return a list of object nodes that compose the functino/terminal set."
  (mapcar #'make-instance node-names)) ;; TODO: error check for undefined nodes

(defun process-fset-arity (fset)
  "Group the functions of the set according to the arity."
  (loop with arity-table = (make-hash-table) 
     for node in fset
     do (let ((nodes (gethash (arity node) arity-table)))
	  (setf (gethash (arity node) arity-table) 
		(push node nodes)))
     finally (return arity-table)))


;;;
;;; function and terminal set definition/creation
;;;

(defmacro defnode (node-name ((&rest args) (&key (string "") (ephemeral nil))) &body code)
  "Define a function/terminal node for a set."
  `(progn
     (defun ,node-name ,args
       ,@code)
     (defclass ,node-name ()
       ((operator :initform ',node-name :reader operator)
	,(if (zerop (length `,args))
	     `(ephemeral :initform ,ephemeral :reader ephemeral)
	     `(arity     :initform ,(length `,args) :reader arity))
	(string :initform ,string :reader string-form)))))


;;;;
;;;; function and terminal sets lists to be used for evolution
;;;;

;;;
;;; function set definitions

;; math
(defnode gp-plus ((a b) (:string "+"))
  (when (and (numberp a) (numberp b))
    (+ a b)))

(defnode gp-minus ((a b) (:string "-"))
  (when (and (numberp a) (numberp b))
    (- a b)))

(defnode gp-times ((a b) (:string "*"))
  (when (and (numberp a) (numberp b))
    (* a b)))

(defnode gp-division ((a b) (:string "/"))
  (when (and (numberp a) (numberp b))
    (if (> b 0)
	(/ a b) b)))

(defnode gp-log ((a) (:string "log"))
  (when (numberp a)
    (log a))) 

(defnode gp-square-root ((a) (:string "sqrt"))
  (when (numberp a)
    (sqrt a)))

(defnode gp-square ((a) (:string "sqr"))
  (when (numberp a)
    (* a a)))	
     
(defnode gp-power ((a b) (:string "pow"))
  (when (and (numberp a) (numberp b))
    (expt a b)))

;; conditionals
(defnode gp-if ((x y z) (:string "if"))
  (if x y z))

(defnode gp-and ((x y) (:string "and"))
  (and x y))

(defnode gp-or ((x y) (:string "or"))
  (or x y))

(defnode gp-not ((x) (:string "not"))
  (not x))

;; comparators
(defnode gp-< ((x y) (:string "<"))
  (and (numberp x) (numberp y)
       (< x y)))

(defnode gp-<= ((x y) (:string "<="))
  (and (numberp x) (numberp y)
       (<= x y)))

(defnode gp-> ((x y) (:string ">"))
  (and (numberp x) (numberp y)
       (> x y)))

(defnode gp->= ((x y) (:string ">="))
  (and (numberp x) (numberp y)
       (>= x y)))

(defnode gp-= ((x y) (:string "="))
  (and (numberp x) (numberp y)
       (= x y)))

(defnode gp-/= ((x y) (:string "/="))
  (and (numberp x) (numberp y)
       (/= x y)))

;; others
(defnode gp-random-n ((n) (:string "random-n"))
  (when (numberp n)
    (random n)))


;;;
;;; terminal set definitions

;; constants (only used in a tree generation by keeping their values)

;; allows a user-defined function to generate constants
;; - store fucntion in *generate-constant*
;; - just add to tset gp-constant

(defparameter *generate-constant* nil)

(defnode gp-constant (() (:string "constant" :ephemeral t))
  (funcall *generate-constant*))

;; specific constant generators (ephemeral constants)
(defnode gp-constant-int (() (:string "int" :ephemeral t))
  (random 10))

(defnode gp-constant-real (() (:string "real" :ephemeral t))
  (random 1.0))

(defnode gp-true (() (:string "true" :ephemeral t)) t)

(defnode gp-false (() (:string "false" :ephemeral t)) nil)

;; random numbers
(defnode gp-random-real (() (:string "random-real" :ephemeral nil))
  (random 1.0))

(defnode gp-random-10 (() (:string "random-10" :ephemeral nil))
  (random 10))

(defnode gp-random-100 (() (:string "random-100" :ephemeral nil))
  (random 100))
