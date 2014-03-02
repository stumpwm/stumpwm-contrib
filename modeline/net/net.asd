;;;; net.asd

(asdf:defsystem #:net
  :serial t
  :description "Describe net here"
  :author "Vitaly Mayatskikh"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "net")))

