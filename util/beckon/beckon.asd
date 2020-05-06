(asdf:defsystem #:beckon
  :description "Beckon the mouse to the current window"
  :author "Mark Dawson <markgdawson@gmail.com>"
  :license  "GNU GPL v3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "beckon"))
  :depends-on (#:stumpwm))
