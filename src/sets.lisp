(in-package #:core-gp)

;;;
;;; function and terminal set definition/creation
;;;

(defmacro defnode (node-name (&rest args) string &body code)
  "Define a function/terminal node for a set."
  `(progn
     (defun ,node-name ,args
       ,@code)
     (defclass ,node-name ()
       ((operator :initform ',node-name    :reader operator)
	(arity    :initform ,(length args) :reader arity)
	(string   :initform ,string        :reader string-form)))))

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


;;;
;;; function and terminal sets lists to be used for evolution
;;;

;;;
;;; function set definitions
;;;

;; math
(defun gp-plus (a b)
  (when (and (numberp a) (numberp b))
    (+ a b)))

(defun gp-minus (a b)
  (when (and (numberp a) (numberp b))
    (- a b)))

(defun gp-times (a b)
  (when (and (numberp a) (numberp b))
    (* a b)))

(defun gp-divison (a b)
  (when (and (numberp a) (numberp b))
    (if (> b 0)
	(/ a b) b)))

(defun gp-log (a)
  (when (numberp a)
    (log a)))	     

(defun gp-square-root (a)
  (when (numberp a)
    (sqrt a)))

(defun gp-square (a)
  (when (numberp a)
    (* a a)))	
     
(defun gp-power (a b)
  (when (and (numberp a) (numberp b))
    (expt a b)))

;; conditionals
(defun gp-if (x y z)
  (if x y z))

(defun gp-and (x y)
  (and x y))

(defun gp-or (x y)
  (or x y))

(defun gp-not (x)
  (not x))

;; comparators
(defun gp-< (x y)
  (and (numberp x) (numberp y)
       (< x y)))

(defun gp-<= (x y)
  (and (numberp x) (numberp y)
       (<= x y)))

(defun gp-> (x y)
  (and (numberp x) (numberp y)
       (> x y)))

(defun gp->= (x y)
  (and (numberp x) (numberp y)
       (>= x y)))

(defun gp-= (x y)
  (and (numberp x) (numberp y)
       (= x y)))

(defun gp-/= (x y)
  (and (numberp x) (numberp y)
       (/= x y)))

;; others
(defun gp-random-n (n)
  (when (numberp n)
    (random n)))


;;;
;;; terminal set definitions
;;;

;; constants (only used in a tree generation by keeping their values)

(defparameter *generate-constant* nil)

(defun gp-constant ()
  (funcall *generate-constant*))

(defun gp-constant-int (&optional (max 100))
  (random max))

(defun gp-constant-real ()
  (random 1.0))

(defun gp-true ()
  t)

(defun gp-false ()
  nil)

;; random numbers
(defun gp-random-real ()
  (random 1.0))

(defun gp-random-10 ()
  (random 10))

(defun gp-random-100 ()
  (random 100))
