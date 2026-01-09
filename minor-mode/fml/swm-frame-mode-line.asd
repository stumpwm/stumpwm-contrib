;;;; swm-frame-mode-line.asd

(asdf:defsystem #:swm-frame-mode-line
  :description "Mode line per frame"
  :author "szos at posteo dot net"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "swm-frame-mode-line")))
