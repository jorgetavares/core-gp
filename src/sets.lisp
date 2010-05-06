(in-package #:core-gp)

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
	(string   :initform ,string :reader string-form)))))

(defun make-set (node-names)
  "Return a list of object nodes that compose the functino/terminal set."
  (mapcar #'make-instance node-names))

(defun process-fset-arity (fset)
  "Group the functions of the set according to the arity."
  (loop with arity-table = (make-hash-table) 
     for node in fset
     do (let ((nodes (gethash (arity node) arity-table)))
	  (setf (gethash (arity node) arity-table) 
		(push node nodes)))
     finally (return arity-table)))



;;;;
;;;; function and terminal sets lists to be used for evolution
;;;;

;;;
;;; function set definitions
;;;

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

(defnode gp-divison ((a b) (:string "/"))
  (when (and (numberp a) (numberp b))
    (if (> b 0)
	(/ a b) b)))

(defnode gp-log ((a) ((:string "log"))
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
;;;

;; constants (only used in a tree generation by keeping their values)

(defparameter *generate-constant* nil)

(defnode gp-constant (() (:string "integer" :ephemeral t))
  (funcall *generate-constant*))

(defnode gp-constant-real (() (:string "real" :ephemeral t))
  (random 1.0))

(defnode gp-true (() (:string "true" :ephemeral t)) t)

(defnode gp-false (() (:string "false" :ephemeral t)) nil)

;; random numbers
(defnode gp-random-real (() (:string "random-real" :ephemeral t))
  (random 1.0))

(defnode gp-random-10 (() (:string "random-10" :ephemeral t))
  (random 10))

(defnode gp-random-100 (() (:string "random-100" :ephemeral t))
  (random 100))
