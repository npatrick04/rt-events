# rt-events

`rt-events` is a simple event notification utility for multi-threaded applications.  It is designed to provide an event interface similar to that provided by VxWorks, RTEMS, or other Real-Time Operating Systems.  This implementation uses a mutex and condition variable, so it cannot be used in an interrupt context as you'd get with a RTOS event manager.  That said, the implementation is simple enough.  Your mileage may vary.

The implementation depends only on Bordeaux Threads, and so is compatible wherever that is.  Thus far it's been tested on SBCL.

# Installation

clone the git repository somewhere ASDF can find it.  

```lisp
(asdf-load-system :rt-events)
```

# Usage

The rt-events package will generate the maximum number of events possible, which is implementation and architecture dependent.  `+max-events+` gives the number of independent events, which are enumerated by `+event-1+` through `+event-XX+` where XX is `+max-events+`.

Make an event object: 

```lisp
(defparameter e (make-instance 'event))
```

Send an event:

```lisp
;; Typically you would define a variable or constant with an informative name...
(send e +event-1+)

;; Send an event to an event object.  If a blocked
;; thread's event condition is satisfied by this event, then it
;; will be unblocked.  
;; 
;; If the event condition is not satisfied and a thread is pending on
;; it, then the events satisfied are updated in the event object and
;; the thread is left pending.  If a thread is not pending on the event
;; object, then the events are updated.
;; 
;; Send returns set.
```

Receive an event:

```lisp
;; Multiple events may be returned in received-event
(let ((received-event (receive e +all-events+ :timeout :wait-forever :condition-type :any)))
  ...)

;; Attempt to receive an event condition specified by SET.  
;; 
;; A CONDITION-TYPE of :ANY and :ALL are used to specify whether a
;; single event in the set or the complete event set is necessary to
;; satisfy the event condition.  
;; 
;; The TIMEOUT parameter is used to specify whether or not the
;; thread will wait for the event condition to be satisfied.  When
;; TIMEOUT is set to :NO-WAIT, the calling thread will evaluate the
;; pending events, returning :UNSATISFIED when the condition is not
;; met.  
;; 
;; When TIMEOUT is :WAIT-FOREVER (default), the calling thread will
;; become blocked if the pending events do not satisfy the event
;; condition set.  It will remain blocked until a send is performed on
;; the set that satisfies the event condition.
;; 
;; When the event condition is met, the return value will be the value
;; corresponding to the events in SET that were satisfied, and the
;; pending events that were satisfied are cleared.  If the thread must
;; wait for the event condition to be satisfied, then a real value in
;; the TIMEOUT parameter is used to specify the maximum duration to
;; wait.
;; 
;; If a timeout is encountered, the BORDEAUX-THREADS:TIMEOUT
;; condition will be raised.  The RETURN-TIMEOUT restart may be used to
;; instead cause a timeout to result in a return value of :TIMEOUT.
```

Events can be pulled out of the received event by bitwise AND:

```lisp
(let ((received-event (receive e +all-events+ :timeout :wait-forever :condition-type :any)))
  (when (plusp (logand received-event +event-1+))
    ...))
```

See examples/simple.lisp.
