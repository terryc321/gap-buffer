;;;; gap.asd

(asdf:defsystem #:gap
  :description "Describe gap here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:fiveam)
  :components ((:file "package")
               (:file "gap")))
