(defpackage #:core-gp-system
  (:use #:common-lisp #:asdf))  
 
(in-package #:core-gp-system)  
 
(defsystem :core-gp
  :description "core-gp: a Genetic Programming library in CL."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "src/package")
	       (:file "src/sets"      :depends-on ("src/package"))
	       (:file "src/tree"      :depends-on ("src/package" "src/sets"))
	       (:file "src/selection" :depends-on ("src/package"))
               (:file "src/core"      :depends-on ("src/package" 
						   "src/sets" 
						   "src/tree" 
						   "src/selection"))))
