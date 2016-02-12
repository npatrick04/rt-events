;;;; events.asd

(asdf:defsystem #:events
  :description "Describe events here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "events")))

