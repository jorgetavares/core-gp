;;;;
;;;; core-gp example: GE "hello world"
;;;;

(defpackage #:core-gp-ge-hello-world
  (:use #:common-lisp #:core-gp)
  (:export #:ge-hello-world 
	   #:*grammar-hello-world*
	   #:out-char
	   #:make-fitness-ge-hello-world
	   ))

(in-package #:core-gp-ge-hello-world)

;;;
;;; grammar
;;;

;; Hello World
;<prog> ::= <func> | <func><prog>
;<func> ::= writeChar( <letter> );
;<letter> ::= <vowel> | <consonant> | <space>
;<space> ::= '_'
;<vowel> ::='a'|'o'|'u'|'e'|'i'
;<consonant> ::= 'q'|'w'|'r'|'t'|'y'|'p'|'s'|'d'|'f'|'g'|'h'|'j'|'k'|'l'|'z'|'x'|'c'|'v'|'b'|'n'|'m'

(defparameter *grammar-hello-world*
  '((<prog>     -> <func> (append <func> <prog>))
    (<func>     -> (out-char <letter>))
    (<letter>   -> <vowel> <consoant> <space>)
    (<space>    -> #\Space)
    (<vowel>    -> #\a #\e #\i #\o #\i)
    (<consoant> -> #\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t #\v #\w #\x #\y #\z)))

;; auxiliary functions
(defun out-char (char)
  (list char))


;;;
;;; fitness function 
;;;

(defun make-fitness-ge-hello-world ()
  (let ((target '(#\h #\e #\l #\l #\o #\Space #\w #\o #\r #\l #\d))
	(target-size 11))
    #'(lambda (candidate-solution)
	(let* ((result (eval candidate-solution))
	       (size (length result))
	       (min-length (if (< size target-size) size target-size)))
	  (+ (loop for i from 0 below min-length
		   unless (eql (nth i result) 
			       (nth i target)) sum 1 into differences
		   finally (return differences))
	     (abs (- target-size size)))))))
	
;;;
;;; run GE
;;;

(defun ge-hello-world (&key (id "ge-hello-world") (output :screen) (generations 100) (pop-size 100) (genome-size 100))
  (core-gp:ge-generic :id id
		      :output output
		      :pop-size pop-size
		      :genome-size genome-size
		      :evaluation-fn (make-fitness-ge-hello-world)
		      :grammar *grammar-hello-world*
		      :mapping-fn #'map-ge
		      :wrap t
		      :elitism t
		      :replacement-mode :generational
		      :terminal-value generations
		      :cx-operator #'one-point-crossover
		      :cx-rate 0.7
		      :mt-operator #'flip-mutation
		      :mt-rate 1.0
		      :mt-gene-rate 0.02
		      :terminal-value generations
		      :comparator #'<
		      ))
