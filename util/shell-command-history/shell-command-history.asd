;;;; shell-command-history.asd

(asdf:defsystem :shell-command-history
  :description "Save and load the stumpwm::*input-shell-history* to a file"
  :author "cage"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:stumpwm)
  :components ((:file "package")
               (:file "shell-command-history")))
