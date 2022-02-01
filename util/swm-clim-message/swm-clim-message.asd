;;;; swm-clim-message.asd

(asdf:defsystem #:swm-clim-message
  :description "Display StumpWM messages and menus through CLIM"
  :author "szos (at) posteo (dot) net"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm #:clim #:clim-lisp #:mcclim)
  :components ((:file "package")
               (:file "swm-clim-message")
               (:file "reimplementations")))
