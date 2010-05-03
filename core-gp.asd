(defpackage #:core-gp-system
  (:use #:common-lisp #:asdf))  
 
(in-package #:core-gp-system)  
 
(defsystem :core-gp
  :description "core-gp: a Genetic Programming library in CL."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "src/package")
	       (:file "src/sets"        :depends-on ("src/package"))
	       (:file "src/tree"        :depends-on ("src/package" 
						     "src/sets"))
	       (:file "src/individual"  :depends-on ("src/package" 
						     "src/tree"))
	       (:file "src/evaluation"  :depends-on ("src/package" 
						     "src/individual"))
	       (:file "src/selection"   :depends-on ("src/package"))
	       (:file "src/crossover"   :depends-on ("src/package"))
	       (:file "src/mutation"    :depends-on ("src/package" 
						     "src/sets"))
	       (:file "src/utilities"   :depends-on ("src/package" 
						     "src/individual"))
	       (:file "src/output"      :depends-on ("src/package" 
						     "src/individual" 
						     "src/utilities"))
	       (:file "src/core"        :depends-on ("src/package" 
						     "src/sets" 
						     "src/tree" 
						     "src/individual"
						     "src/evaluation"
						     "src/selection"
						     "src/crossover"
						     "src/mutation"
						     "src/utilities"
						     "src/output"))))
						    
