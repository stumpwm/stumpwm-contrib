;;;; productivity.asd

(asdf:defsystem #:productivity
  :serial t
  :description "Lock StumpWM down so you have to get work done."
  :author "Ivy Foster"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "productivity")))

