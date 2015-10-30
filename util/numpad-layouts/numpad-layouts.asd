;;;; numpad-layouts.asd

(asdf:defsystem #:numpad-layouts
  :serial t
  :description "A module for handling different keyboards numpad layouts"
  :author "David Bjergaard"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "numpad-layouts")))

