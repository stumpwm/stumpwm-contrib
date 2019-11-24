;;;; disk.asd

(asdf:defsystem #:disk
  :serial t
  :description "Display filesystem information in the modeline"
  :author "Morgan Veyret"
  :license "GPLv3"
  :depends-on (:stumpwm
               :cl-diskspace)
  :components ((:file "package")
               (:file "disk")))
