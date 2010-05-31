(defpackage #:core-gp-examples-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:core-gp-examples-system)  
 
(defsystem :core-gp-examples
  :description "core-gp-examples: examples of the :core-gp."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"
  :depends-on (core-gp)  
  :components ((:file "examples/package")
	       (:file "examples/onemax"       :depends-on ("examples/package"))
               (:file "examples/regression"   :depends-on ("examples/package"))
	       (:file "examples/sin-function" :depends-on ("examples/package"))))
