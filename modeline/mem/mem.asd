;;;; mem.asd

(asdf:defsystem #:mem
  :serial t
  :description "Display memory in the modeline, %M conflicts with maildir."
  :author "Vitaly Mayatskikh"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "mem")))

