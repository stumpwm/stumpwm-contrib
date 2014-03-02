;;;; maildir.asd

(asdf:defsystem #:maildir
  :serial t
  :description "Describe maildir here"
  :author "Morgan Veyret"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "maildir")))

