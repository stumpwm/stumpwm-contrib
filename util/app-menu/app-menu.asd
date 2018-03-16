;;;; app-menu.asd

(asdf:defsystem #:app-menu
  :serial t
  :description "A simple application menu for launching shell commands"
  :author "Anonymous"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "app-menu")))

