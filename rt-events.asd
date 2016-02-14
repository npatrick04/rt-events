;;;; events.asd

(asdf:defsystem #:rt-events
  :description "A simple real-time events API."
  :long-description "This package provides a simple way to communicate
  event occurances between different threads."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :depends-on (#:bordeaux-threads)
  :serial t
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "rt-events")))))

