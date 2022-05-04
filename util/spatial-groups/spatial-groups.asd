;;;; spatial-groups.asd

(asdf:defsystem #:spatial-groups
  :description "Spatial Groups navigation for StumpWM"
  :author "Russell Adams <rladams@adamsinfoserv.com>"
  :license  "GPL-v2"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "spatial-groups")))
