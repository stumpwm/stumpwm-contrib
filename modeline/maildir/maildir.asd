;;;; maildir.asd

(asdf:defsystem #:maildir
  :serial t
  :description "Display maildir information in the modeline (%M conflicts with mem). "
  :author "Morgan Veyret"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "maildir")))

