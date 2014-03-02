;;;; undocumented.asd

(asdf:defsystem #:undocumented
  :serial t
  :description "Describe undocumented here"
  :author "Ben Spencer"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "undocumented")))

