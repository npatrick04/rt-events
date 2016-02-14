;;;; rt-events.examples.asd

(asdf:defsystem #:rt-events.examples
  :description "Example use cases for events."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:rt-events)
  :serial t
  :components ((:module "examples"
			:components
			((:file "package")
			 (:file "simple")))))
