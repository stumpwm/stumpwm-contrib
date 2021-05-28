;;;; hidden.asd

(asdf:defsystem #:hidden
  :serial t
  :description "Add hidden window info to the modeline."
  :author "Woodrow Douglass"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "hidden")))

