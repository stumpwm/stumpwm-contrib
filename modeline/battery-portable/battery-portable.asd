;;;; battery-portable.asd

(asdf:defsystem #:battery-portable
  :serial t
  :description "Add battery information to the modeline in a portable way."
  :author "Julian Stecklina"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "battery-portable")))

