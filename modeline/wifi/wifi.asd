;;;; wifi.asd

(asdf:defsystem #:wifi
  :serial t
  :description "Display information about your wifi."
  :author "John Li"
  :license "GPLv3"
  :depends-on (#:stumpwm
               #:alexandria)
  :components ((:file "package")
               (:file "wifi")))
