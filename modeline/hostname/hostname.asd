;;;; hostname.asd

(asdf:defsystem #:hostname
  :description "Put hostname in the StumpWM modeline"
  :author "Joseph Mingrone <jrm@ftfl.ca>"
  :license "2-CLAUSE BSD (see COPYRIGHT file for details)"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "hostname")))

