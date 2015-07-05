;;;; battery.asd

(asdf:defsystem #:battery
  :description "Describe battery here"
  :author "Vitaly Mayatskikh"
  :maintainer "Julian Stecklina"
  :license "GPLv3"
  :version "0.2.0"
  :depends-on (#:stumpwm
               #:cl-ppcre
               #:uiop)
  :components ((:file "battery")))

