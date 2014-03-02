;;;; app-menu.asd

(asdf:defsystem #:app-menu
  :serial t
  :description "Describe app-menu here"
  :author "Anonymous"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "app-menu")))

