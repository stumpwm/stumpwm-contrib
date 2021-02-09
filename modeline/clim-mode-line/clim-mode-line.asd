;;;; clim-mode-line.asd

(asdf:defsystem #:clim-mode-line
  :description "A modeline inspired panel written with CLIM"
  :author "Shozo <szos at posteo dot net>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:clim #:clim-lisp #:mcclim #:slim #:stumpwm)
  :components ((:file "package")
	       (:file "clim-clx-patch")
               (:file "clim-mode-line")
	       (:file "mode-line-formatters")
	       (:file "stumpwm-patch")))
