;;;; package.lisp

(defpackage #:events
  (:use #:cl)
  (:export +max-events+
	   +all-events+
	   +no-events+

	   event
	   wait-type-t
	   *default-wait-type*

           pop-event
	   
	   send
	   receive))

