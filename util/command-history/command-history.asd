;;;; command-history.asd

(asdf:defsystem #:command-history
  :description "Save and load the stumpwm::*input-history* to a file"
  :author "Arjen Dijkstra"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "command-history")))
