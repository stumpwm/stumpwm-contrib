;;;; which-key.asd

(asdf:defsystem #:which-key
  :description "which-key-mode for StumpWM"
  :author "Dany Haddad <danyhaddad43@gmail.com>"
  :license "Eclipse License"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "which-key")))
