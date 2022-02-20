;;;; clim-mode-line.asd

(asdf:defsystem #:clim-mode-line
  :description "A modeline written in CLIM"
  :author "szos at posteo dot net"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:clim #:clim-lisp #:slim #:stumpwm)
  :components ((:file "package")
               (:file "patch-clim-clx")
               (:file "macros")
               (:file "clim-mode-line")
               (:file "presentations")
               (:file "formatters")
               (:file "gestures")
               (:file "commands-and-translators")))
