(defpackage #:core-gp-system
  (:use #:common-lisp #:asdf))  
 
(in-package #:core-gp-system)  
 
(defsystem :core-gp
  :description "core-gp: a Genetic Programming library in CL."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "src/package")
               (:file "src/core")))
