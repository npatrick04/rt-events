;;;; package.lisp

(defpackage #:events
  (:use #:cl)
  (:export +max-events+
	   +all-events+

	   event
	   wait-type-t
	   *default-wait-type*
	   
	   send
	   recv))

