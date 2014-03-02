;;;; mem.asd

(asdf:defsystem #:mem
  :serial t
  :description "Describe mem here"
  :author "Vitaly Mayatskikh"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "mem")))

