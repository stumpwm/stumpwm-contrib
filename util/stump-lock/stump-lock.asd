(defsystem "stump-lock"
  :serial t
  :description "Screen locker in StumpWM"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "lock")))
