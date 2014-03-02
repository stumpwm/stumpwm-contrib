;;;; battery-portable.asd

(asdf:defsystem #:battery-portable
  :serial t
  :description "Describe battery-portable here"
  :author "Julian Stecklina"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "battery-portable")))

