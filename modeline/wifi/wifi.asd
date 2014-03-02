;;;; wifi.asd

(asdf:defsystem #:wifi
  :serial t
  :description "Describe wifi here"
  :author "John Li"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "wifi")))

