(in-package #:core-gp)

;;;
;;; strong-type tree utilities
;;;

(defparameter *stgp-node-types* 
  '(:none 
    (:any :number :integer :real :boolean :none) 
    (:number :integer :real)
    :boolean 
    ))


;;;
;;; sets definition
;;;

(defmacro defstnode (node-name ((&rest args) 
				(&key (string "") (return-type nil) (args-type nil))) 
		     &body code)
  "Define a function node for a Strong-Type set."
  `(progn
     (defun ,node-name ,args ,@code)
     (defclass ,node-name ()
       ((operator    :initform ',node-name      :reader operator)
	(arity       :initform ,(length `,args) :reader arity)
	(args-type   :initform ,args-type       :reader args-type)
	(return-type :initform ,return-type     :reader return-type)
	(string      :initform ,string          :reader string-form)))))
     

(defmacro defstnode-macro (node-name ((&rest args) (&key (string "") (args-type nil))) 
		     &body code)
  "Define a macro node for a Strong-Type set."
  `(progn
     (defmacro ,node-name ,args ,@code)
     (defclass ,node-name ()
       ((operator    :initform ',node-name      :reader operator)
	(arity       :initform ,(length `,args) :reader arity)
	(args-type   :initform ,args-type       :reader args-type)
	(return-type :initform t                :reader return-type)
	(string      :initform ,string          :reader string-form)))))
     

(defmacro defstterm (node-name (&key (string "") (ephemeral nil)
				     (return-type nil)) 
		     &body code)
  "Define a terminal node for a Strong-Type set."
  `(progn
     (defun ,node-name () ,@code)
     (defclass ,node-name ()
       ((operator :initform ',node-name :reader operator)
	(ephemeral :initform ,ephemeral :reader ephemeral)
	(return-type :initform ,return-type :reader return-type)
	(string :initform ,string :reader string-form)))))

;; needed because when doing CX or MT and an ephemeral 
;; constant is selected, we need to know the type.
;; TODO: change the entire tree structure representation
(defclass terminal ()
  ((value :initarg :value :initform nil :reader value)
   (rtype :initarg :rtype :initform nil :reader rtype)))

(defmethod print-object ((object terminal) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (value rtype) object
      (format stream "~a ~a" value rtype)))) 

(defun make-sets-container-st (function-names terminal-names types-tree)
  "Return a container for the Function and Terminal Sets (nodes must be defined)."
  (make-instance 'sets-container
		 :functions (make-set function-names)
		 :terminals (make-set terminal-names)
		 :types-tree types-tree))

(defun process-fset-types (types set)
  "Group the nodes according to return type."
  (let ((node-table (make-hash-table)))
    (loop for node in set
       do (let ((nodes (gethash (return-type node) node-table)))
	    (setf (gethash (return-type node) node-table)
		  (push node nodes))))
    ;; TODO: change to a tree of types will imply recoding this part
    (loop for type in types
       do (when (listp type)
	    (let ((main-type (first type))
		  (subtypes (rest type)))
	      (loop for subtype in subtypes 
		 do (let ((nodes (gethash main-type node-table)))
		      (setf (gethash main-type node-table)
			    (append nodes (gethash subtype node-table)))))))
       finally (return node-table))))

;;;
;;; tree generators and related functions
;;;


(defun ramped-half-and-half-st (size limit fset fset-size tset tset-size fset-types tset-types 
				&optional (use-type nil))
  "A gp tree is created with half of probability for each method."
  (if (< (random 1.0) 0.5)
      (full-method-depth-st size limit fset fset-size tset tset-size 
			    fset-types tset-types use-type)
      (grow-method-depth-st size limit fset fset-size tset tset-size 
			    fset-types tset-types use-type)))


(defun full-method-depth-st (size limit fset fset-size tset tset-size fset-type tset-type 
			     &optional use-type)
  (if (= size limit)
      (let* ((valid-nodes (gethash use-type tset-type))
	     (valid-size (length valid-nodes)))
	(process-terminal-st (nth (random valid-size) valid-nodes)))
      (let ((function (if (null use-type)
			  (nth (random fset-size) fset)
			  ((lambda (nodes)
			     (nth (random (length nodes)) nodes)) 
			   (gethash use-type fset-type)))))
	(cons (operator function)
	      (loop for type in (args-type function)
		 collect (full-method-depth-st 
			  (1+ size) limit fset fset-size tset tset-size
			  fset-type tset-type type))))))
	     
(defun process-terminal-st (node)
  "Process the type of terminal."
  (if (ephemeral node)
      (let ((value (funcall (operator node))))
	(make-instance 'terminal 
		       :value value 
		       :rtype (return-type node)))
      (list (operator node))))


(defun grow-method-depth-st (size limit fset fset-size tset tset-size fset-type tset-type 
			     &optional (use-type nil))
  (if (= size limit)
      (let* ((valid-nodes (gethash use-type tset-type))
	     (valid-size (length valid-nodes)))
	(process-terminal-st (nth (random valid-size) valid-nodes)))
      (let ((index (random (+ fset-size tset-size))))
	(if (< index fset-size) 
	    (let ((function (if (null use-type)
				(nth index fset)
				((lambda (nodes)
				   (nth (random (length nodes)) nodes)) 
				 (gethash use-type fset-type)))))
	      (cons (operator function)
		    (loop  for type in (args-type function)
		       collect (grow-method-depth-st
				(1+ size) limit fset fset-size tset tset-size 
				fset-type tset-type type))))
	    (let ((terminal (if (null use-type)
				(nth (- index fset-size) tset)
				((lambda (nodes)
				   (nth (random (length nodes)) nodes)) 
				 (gethash use-type tset-type)))))
	      (process-terminal-st terminal))))))


;;
;; STGP Tree Mutation
;;

(defgeneric stgp-tree-mutation (genome config)
  (:documentation "STGP tree mutation operator."))

(defmethod stgp-tree-mutation ((genome tree-genome) config)
  "STGP tree mutation: replaces a random subtree with a new random one."
  (setf (chromossome genome)
	(stgp-tree-mutate (chromossome genome) 
			  (mt-gene-rate (operators config)) 
			  (sets (extra-configurations config)) 
			  nil (upper-bound (extra-configurations config)) 0))
  genome)

(defun stgp-tree-mutate (tree rate sets type limit size)
  "STGP tree mutation: replaces a random subtree with a new random one."
  (if (< (random 1.0) rate)
      (ramped-half-and-half-st 0 (- limit size)
			       (functions sets) 
			       (functions-size sets)
			       (terminals sets) 
			       (terminals-size sets)
			       (functions-types-table sets) 
			       (terminals-types-table sets)
			       type)
      (if (and (consp tree)
	       (rest tree))
	  (let* ((node-name (first tree))
		 (function (find-function-node node-name sets))
		 (function-arity (arity function))
		 (function-args (args-type function)))
	    (cons node-name
		  (loop 
		     for arg from 1 to function-arity
		     for arg-type in function-args 
		     collect (stgp-tree-mutate (nth arg tree) 
					       rate sets arg-type limit (1+ size)))))
	  tree)))

;;
;; STGP Tree Crossover
;;

(defgeneric stgp-tree-crossover (genome1 genome2 config)
  (:documentation "STGP Tree crossover operator."))

(defmethod stgp-tree-crossover ((genome1 tree-genome) (genome2 tree-genome) config)
  (multiple-value-bind (o1 o2)
      (cross-subtrees-st (chromossome genome1) 
			 (chromossome genome2) 
			 (maximum-size (population config))
			 (sets (extra-configurations config)))
    (values (make-instance 
	     'individual 
	     :id (generate-id)
	     :genome (make-tree-genome
		      (copy-tree o1) (max-tree-depth o1) (count-tree-nodes o1))
	     :fitness (make-fitness (fitness-type (evaluation config))))
	    (make-instance 
	     'individual 
	     :id (generate-id)
	     :genome (make-tree-genome
		      (copy-tree o2) (max-tree-depth o2) (count-tree-nodes o2))
	     :fitness (make-fitness (fitness-type (evaluation config)))))))

(defun cross-subtrees-st (p1 p2 depth sets)
  "Exchanges two subtrees with types in a random point."
  (let* ((p1-point (random (count-tree-nodes p1)))
	 (o1 (list (copy-tree p1)))
         (o2 (list (copy-tree p2))))
    (multiple-value-bind (p1-subtree p1-fragment)
        (get-subtree-st (first o1) o1 p1-point)
      (let* ((node-name (if (listp p1-fragment) 
			    (first p1-fragment) p1-fragment))
	     (node-type 
	      (if (find-function-node node-name sets) ;; TODO: use aif macro
		  (return-type (find-function-node node-name sets))
		  (let ((ttype (find-terminal-node node-name sets)))
		    (if ttype 
			(return-type ttype)   ;; if the terminal is non-ephemeral, return it
			(rtype node-name))))) ;; else the type is know because 
	                                      ;; its stored with the value (terminal class)
	                                      ;; -- this needs to be redone...	    
	     (p2-valid-points (get-valid-points p2 node-type sets))
	     (total-valid-points (length p2-valid-points)))
	(unless (zerop total-valid-points)
	  (multiple-value-bind (p2-subtree p2-fragment)
	      (get-subtree-st (first o2) o2 
			      (nth (random total-valid-points) p2-valid-points))
	    (setf (first p1-subtree) p2-fragment)
	    (setf (first p2-subtree) p1-fragment)))))
    (validate-crossover p1 o1 p2 o2 depth)))

(defun get-subtree-st (tree point index)
  "Return a subtree."
 (if (= index 0)
     (values point tree index)
     (if (consp tree)
	 (do* ((tree-rest (rest tree) (rest tree-rest))
	       (arg (first tree-rest) (first tree-rest)))
	      ((not tree-rest) (values nil nil index))
	   (multiple-value-bind
		 (new-point new-tree new-index)
	       (get-subtree-st arg tree-rest (1- index))
	     (if (= new-index 0)
		 (return (values new-point new-tree new-index))
		 (setf index new-index))))
	 (values nil nil index))))

;(defun get-subtree-st (tree point index)
;  (if (zerop index)
;      (values point tree index)
;      (if (consp tree)
;	  (loop for args in (rest tree)
;	     do (multiple-value-bind (new-point new-tree new-index)
;		    (get-subtree-st (first args) args (1- index))
;		  (if (zerop index)
;		      (return (values new-point new-tree new-index))
;		      (setf index new-index)))
;	     finally (values nil nil index))
;	  (values nil nil index))))

(defun get-valid-points (parent node-type sets)
  "Return the positions of the nodes in the tree that satisfy node-type."
  (let ((index 0))
    (labels ((getpoints (tree)
	       (unless (atom tree)
		 (loop for arg in tree
		    append (if (atom arg)
			       (progn
				 (setf index (1+ index))
				 (when (eql node-type
					    ((lambda (test) ;; TODO: change to aif macro
					       (if test 
						   test
						   (find-terminal-node arg sets)))
					     (find-function-node arg sets)))
				   (list index)))
			       (getpoints arg))))))
      (getpoints parent))))


;;;
;;; Some Strong-Type standard nodes
;;;

;;reals
;; - ephemeral reals
(defstterm stgp-constant-reals (:return-type :real 
			       :ephemeral t
			       :string "reals")
  (random 1.0))

;;ints
;; - ephemeral ints
(defstterm stgp-constant-ints (:return-type :integer 
			      :ephemeral t
			      :string "ints")
  (funcall core-gp:*generate-constant*))

;;booleans
;; - ephemeral booleans
(defstterm stgp-constant-booleans (:return-type :boolean
				   :ephemeral t
				   :string "booleans")
  (if (< (random 1.0) 0.5) t nil))


;; Basic math operators
(defstnode stgp-plus ((x y) (:return-type :number
			     :args-type '(:number :number)
			     :string "plus")) 
  (+ x y))

(defstnode stgp-minus ((x y) (:return-type :number
			     :args-type '(:number :number)
			     :string "minus")) 
  (- x y))

(defstnode stgp-times ((x y) (:return-type :number
			     :args-type '(:number :number)
			     :string "mult")) 
  (* x y))

(defstnode stgp-if ((x y z) (:return-type :any
			     :args-type '(:boolean :any :any)
			     :string "if")) 
  (if x y z))

(defstnode stgp-and ((x y) (:return-type :boolean
			    :args-type '(:boolean :boolean)
			    :string "and")) 
  (and x y))

(defstnode stgp-or ((x y) (:return-type :boolean
			   :args-type '(:boolean :boolean)
			   :string "or")) 
  (or x y))

(defstnode stgp-equal ((x y) (:return-type :boolean
		 	     :args-type '(:number :number)
			     :string "=")) 
  (= x y))

(defstnode stgp-less ((x y) (:return-type :boolean
		 	     :args-type '(:number :number)
			     :string "<")) 
  (< x y))

(defstnode stgp-greater ((x y) (:return-type :boolean
		 	       :args-type '(:number :number)
			       :string ">")) 
  (> x y))

(defstnode stgp-prog2 ((x y) (:return-type :none
		 	     :args-type '(:any :any)
			     :string "prog2")) 
  (progn x y))

(defstnode stgp-prog3 ((x y z) (:return-type :none
		 	      :args-type '(:any :any :any)
			      :string "prog3")) 
  (progn x y z))
