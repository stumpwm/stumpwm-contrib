;;;; disk.asd

(asdf:defsystem "disk"
  :serial t
  :description "Display filesystem information in the modeline"
  :author "Morgan Veyret"
  :license "GPLv3"
  :depends-on (:stumpwm
               :cl-diskspace
               (:feature :linux "cl-mount-info"))
  :components ((:file "package")
               (:file "disk")))
