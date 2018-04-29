(asdf:defsystem #:pinentry
  :serial t
  :description "Integrate GnuPG Agent with StumpWM"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on (:cffi :stumpwm :usocket-server :percent-encoding)
  :components ((:file "pinentry")))
