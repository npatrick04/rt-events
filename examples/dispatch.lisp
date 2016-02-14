(in-package :rt-events.examples)

(defvar +first-event+  +event-1+)
(defvar +second-event+ +event-2+)
(defvar +quit-event+   +event-3+)

(defun event-handler (seconds event set)
  "After seconds elapse, send an event.  This is supposed to represent
  some kind of external event occurring."
  (sleep seconds)
  (send event set))

(defun event-1-dispatch ()
  (format t "Dispatching event 1~%"))
(defun event-2-dispatch ()
  (format t "Dispatching event 2~%"))
(defun quit-dispatch ()
  :quit)

(defun dispatch ()
  "Use the pop-event-index function to dispatch each event."
  (let ((e (make-instance 'event))
	(handlers (make-array 3 :initial-contents '(event-1-dispatch
						    event-2-dispatch
						    quit-dispatch))))
    (bt:make-thread (lambda ()
		      ;; Send two events simultaneously!
		      (event-handler 1.0 e (logior +second-event+
						   +first-event+))
		      (event-handler 1.0 e +quit-event+)))

    ;; Timeout in this case will
    ;; raise a condition.  There
    ;; should be no timeout in this
    ;; test. 
    (do	((received-events (receive e +all-events+
				   :condition-type :any
				   :timeout 2)
			  (receive e +all-events+
				   :condition-type :any
				   :timeout 2)))
	((plusp (logand received-events +quit-event+))
	 'test-complete)
      (do ((event-index (pop-event-index received-events)
			(pop-event-index received-events)))
	  ((eq event-index +max-events+))
	(funcall (elt handlers event-index))))))
