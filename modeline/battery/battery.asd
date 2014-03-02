;;;; battery.asd

(asdf:defsystem #:battery
  :serial t
  :description "Describe battery here"
  :author "Vitaly Mayatskikh"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "battery")))

