;;;; disk.asd

(asdf:defsystem #:disk
  :serial t
  :description "Describe disk here"
  :author "Morgan Veyret"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "disk")))

