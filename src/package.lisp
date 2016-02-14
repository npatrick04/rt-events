;;;; package.lisp

(defpackage #:rt-events
  (:use #:cl)
  (:export +max-events+
	   +all-events+
	   +no-events+

	   event
	   condition-type-t
	   *default-condition-type*

           pop-event
           pop-event-index
	   bit->event
	   event-number->event
	   
	   send
	   receive

	   ;; The timeout and restart
	   return-timeout))

