(defpackage #:core-gp-examples-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:core-gp-examples-system)  
 
(defsystem :core-gp-examples
  :description "core-gp-examples: examples of core-gp."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"
  :depends-on (core-gp cl-tsplib)  
  :components ((:file "examples/onemax")
	       (:file "examples/ninemax")
	       (:file "examples/tsp")
               (:file "examples/regression")
	       (:file "examples/sin-function")
	       (:file "examples/stgp")))
