;;;; productivity.asd

(asdf:defsystem #:productivity
  :serial t
  :description "Describe productivity here"
  :author "Ivy Foster"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "productivity")))

