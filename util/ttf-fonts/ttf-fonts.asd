;;;; ttf-fonts.asd

(asdf:defsystem #:ttf-fonts
  :serial t
  :description "A pure lisp implementation of TTF font rendering."
  :author "Michael Filonenko"
  :license "GPLv3"
  :depends-on (#:stumpwm #:clx-truetype)
  :components ((:file "package")
               (:file "ttf-fonts")))

