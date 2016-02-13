;; A simple example of events.
(in-package #:rt-events.examples)

(defvar +some-special-event+ +event-1+)
(defvar +some-other-event+   +event-2+)
(defvar +some-regular-event+ +event-3+)

(defun event-handler (seconds event set)
  "After seconds elapse, send an event.  This is supposed to represent
  some kind of external event occurring."
  (sleep seconds)
  (send event set))

(defun simple ()
  "A typical use case for events.  The event-handler is simulating
  some external API call, another process, or whatever else might
  need to trigger event handling in this thread."
  (let ((e (make-instance 'event)))
    (bt:make-thread (lambda ()
		      (event-handler 1.0 e +some-special-event+)
		      (event-handler 1.0 e +some-other-event+)
		      (event-handler 1.0 e +some-regular-event+)))

    (handler-bind ((bt:timeout #'return-timeout))
       (loop
	(let ((received-events (receive e +all-events+
					:condition-type :any
					:timeout 2.0)))

	  (when (eq received-events :timeout)
	    (format t "Timeout!~%")
	    (force-output)
	    (return))
	  ;; TODO: Could also use trivia matching...
	  (when (plusp (logand received-events +some-special-event+))
	    (format t "Got +some-special-event+~%"))
	  (when (plusp (logand received-events +some-other-event+))
	    (format t "Got +some-other-event+~%"))
	  (when (plusp (logand received-events +some-regular-event+))
	    (format t "Got +some-regular-event+~%")))))))
