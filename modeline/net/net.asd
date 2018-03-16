;;;; net.asd

(asdf:defsystem #:net
  :serial t
  :description "Displays information about the current network connection."
  :author "Vitaly Mayatskikh"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "net")))

