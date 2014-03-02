;;;; passwd.asd

(asdf:defsystem #:passwd
  :serial t
  :description "Describe passwd here"
  :author "Anonymous"
  :license "GPLv3"
  :depends-on (#:stumpwm #:ironclad)
  :components ((:file "package")
               (:file "passwd")))

