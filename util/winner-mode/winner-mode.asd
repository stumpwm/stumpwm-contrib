(asdf:defsystem #:winner-mode
  :serial t
  :description "Emacs' winner-mode for StumpWM"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on (:stumpwm)
  :components ((:file "package")
               (:file "macros")
               (:file "variables")
               (:file "dumper")
               (:file "winner-mode")))
