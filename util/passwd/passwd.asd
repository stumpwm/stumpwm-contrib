;;;; passwd.asd

(asdf:defsystem "passwd"
  :serial t
  :description "A simple password utility based on ironclad."
  :author "Anonymous"
  :license "GPLv3"
  :depends-on ("stumpwm"
               "ironclad")
  :components ((:file "package")
               (:file "passwd")))

