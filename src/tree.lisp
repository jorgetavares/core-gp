(in-package #:core-gp)

;;;
;;; tree generators and related functions
;;;

(defun ramped-half-and-half (limit fset tset)
  "A gp tree is created with half of probability for each method."
  (let ((fset-size (length fset))
	(tset-size (length tset)))
    (if (< (random 1.0) 0.5)
	(full-method-tree-generic 0 limit fset fset-size tset tset-size)
	(grow-method-tree-generic 0 limit fset fset-size tset tset-size))))

(defun make-fset (&rest args)
  (loop 
     for function in args by #'cddr
     for arguments in (rest args) by #'cddr
     collect (cons function arguments)))

(defun function-name (pair)
  (car pair))

(defun function-args (pair)
  (cdr pair))

(defun full-method-tree-generic (size limit fset fset-size tset tset-size)
  "Random tree according to the Full method."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let* ((function (nth (random fset-size) fset))
	     (name (function-name function))
	     (args (function-args function)))
	(cons name
	      (loop repeat args 
		 collect (full-method-tree-generic 
			  (1+ size) limit fset fset-size tset tset-size))))))

(defun grow-method-tree-generic (size limit fset fset-size tset tset-size)
  "Random tree according to the Grow method."
  (if (= size limit)
      (process-terminal (nth (random tset-size) tset))
      (let* ((set (append fset tset))
	     (element (nth (random (+ fset-size tset-size)) set)))
	(if (consp element)
	    (let ((name (function-name element))
		  (args (function-args element)))
	      (cons name
		    (loop repeat args 
		       collect (grow-method-tree-generic 
				(1+ size) limit fset fset-size tset tset-size))))
	    (process-terminal element)))))

(defun process-terminal (terminal)
  "Return a constant or a terminal function."
  (case terminal
    (gp-constant (gp-constant))
    (gp-constant-int (gp-constant-int))
    (gp-constant-real (gp-constant-real))
    (gp-true (gp-true))
    (gp-false (gp-false))
    (otherwise (list terminal))))
