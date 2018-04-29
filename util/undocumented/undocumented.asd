;;;; undocumented.asd

(asdf:defsystem #:undocumented
  :serial t
  :description " Look for stuff that should probably be in the manual that isn't"
  :author "Ben Spencer"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "undocumented")))

