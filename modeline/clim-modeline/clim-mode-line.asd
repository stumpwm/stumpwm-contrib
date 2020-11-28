;;;; clim-mode-line.asd

(asdf:defsystem #:clim-mode-line
  :description "Describe clim-mode-line here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clim #:clim-lisp #:mcclim #:slim #:stumpwm)
  :components ((:file "package")
	       (:file "clim-clx-patch")
               (:file "clim-mode-line")
	       (:file "mode-line-formatters")
	       (:file "stumpwm-patch")))
