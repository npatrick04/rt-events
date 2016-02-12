(in-package #:events)

(defmacro define-constant (name value &optional doc)
  (declare (optimize debug))
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Define the implementation/platform specific constants.
  ;; There's probably a better way to do this...

  (define-constant +max-events+ (integer-length most-positive-fixnum)
    "This implementation can pass up to this many events."))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun event-number->event (value)
    "Convert the event number (as in the 1 in +event-1+) to the
  associated event value.  The valid input range for this function is
  from 1 to +max-events+."
    (ash 1 (- +max-events+ value)))

  (defun define-event (value format-string)
    (declare (optimize debug))
    (let ((symb (intern (format nil format-string value))))
      (list
       (list 'define-constant symb (event-number->event value))
       (list 'export (list 'quote symb) (list 'find-package :events)))))
  (defmacro define-event-set (min format-string)
    (let ((range (loop for i from min to +max-events+ collect i)))
      `(progn
         ,@(mapcan (lambda (val) (define-event val format-string))
                   range))))

  (define-event-set 1 "+EVENT-~D+")

  (define-constant +all-events+ (1- (ash 1 #.+max-events+)))
  (define-constant +no-events+ 0)
  )

(defun bit->event (bit)
  (declare (type fixnum bit)
           (optimize speed))
  (event-number->event (- +max-events+ bit -1)))

(defun pop-event (set)
  "Extract the first event in the set, returning it and the remainder values."
  (declare (type fixnum set))
  (let ((first-event (bit->event (integer-length set))))
    (values first-event
            (logxor first-event set))))

;; The event object is the thing that manages communication of
;; events between threads. 

(deftype condition-type-t ()
  `(member :any :all))
(defvar *default-condition-type* :all)

(defclass event ()
  ((receive-set :accessor event-receive-set
                :initform 0
                :type 'fixnum
                :documentation "The current events available for receipt")
   (condition-type :accessor event-condition-type
		   :initform *default-condition-type*
		   :type 'condition-type-t)
   (wait-set :accessor event-wait-set
	     :initform 0
	     :type 'fixnum
	     :documentation "The set used to apply the wait type to~
                             determine whether or not to return.")
   (lock :accessor event-lock
	 :initform (bt:make-lock))
   (ready :accessor event-ready
	  :initform (bt:make-condition-variable))))

(defgeneric send (event set)
  (:documentation "Send an event set to an event object.  If a blocked
  thread's event condition is satisfied by this event set, then it
  will be unblocked.  If the event condition is not satisfied and a
  thread is pending on it, then the events satisfied are updated in
  the event object and the thread is left pending.  If a thread is not
  pending on the event object, then the events are left pending."))
(defgeneric receive (event set &key timeout condition-type)
  (:documentation "Attempt to receive an event condition specified by
  SET.  The TIMEOUT parameter is used to specify whether or not the
  thread will wait for the event condition to be satisfied.  :ANY and
  :ALL are used in the CONDITION-TYPE to specify whether a single
  event or the complete event set is necessary to satisfy the event
  condition.  When the event condition is met, the return value will
  be the value corresponding to the events in SET that were satisfied.

  If pending events satisfy the event condition, then the return value
  is the satisfied events, and the pending events that were satisfied
  are cleared.  If the event condition is not satisfied and :NO-WAIT
  is specified in TIMEOUT, then the return value is :UNSATISFIED.  If
  the event condition is not satisfied and :WAIT-FOREVER is specified
  in TIMEOUT, then the thread will block waiting for the event
  condition.  If the thread must wait for the event condition to be
  satisfied, then a real value in the timeout parameter is used to
  specify the maximum duration to wait.

  If a timeout is specified, and the thread waits for the specified
  duration without an event, the return value will be :TIMEOUT."))

(defun condition-met (events waiting-for type)
  (let ((result (logand events waiting-for)))
    (ecase type
      (:any (when (plusp result) result))
      (:all (when (eq result waiting-for) result)))))

(defmethod send ((e event) set)
  (declare (type fixnum set))
  (with-accessors ((lock event-lock)
		   (ready event-ready)
		   (condition-type event-condition-type)
		   (receive-set event-receive-set)
		   (wait-set event-wait-set)) e
    (bt:with-lock-held (lock)
      (setf receive-set (logior set receive-set))
      (when (condition-met receive-set wait-set
                           condition-type)
        (bt:condition-notify ready))
      set)))

(defun %receive (e set timeout condition)
  (with-accessors ((lock event-lock)
		   (ready event-ready)
		   (condition-type event-condition-type)
		   (receive-set event-receive-set)
		   (wait-set event-wait-set)) e
    (bt:with-lock-held (lock)
      (let ((result (condition-met receive-set set
                                   condition)))
        (if result
            (progn ;; Already good to go
              (setf receive-set (logxor
                                 receive-set
                                 result))
              result)
            ;; Need to condition for the
            ;; trigger after setting it
            ;; up.
            (if (eq timeout :NO-WAIT)
                :UNSATISFIED
                (progn
                  (setf wait-set set
                        condition-type condition)
                  (bt:condition-wait ready
                                     lock
                                     :timeout
                                     (unless
                                         (eq timeout :wait-forever) 
                                       timeout))
                  (let ((result (condition-met receive-set set
                                               condition)))
                    (setf receive-set (logxor
                                       receive-set
                                       result))
                    result))))))))

(defmethod receive ((e event) set &key (timeout :wait-forever)
                                    (condition-type *default-condition-type*))
  (declare (type (or keyword real) timeout))
  (assert (plusp set))
  (if (realp timeout)
      (handler-case
          (bt:with-timeout (timeout)
            (%receive e set timeout condition-type))
        (bt:timeout () :timeout))
      (%receive e set timeout condition-type)))
