;;;; ttf-fonts.asd

(asdf:defsystem #:ttf-fonts
  :serial t
  :description "Describe ttf-fonts here"
  :author "Michael Filonenko"
  :license "GPLv3"
  :depends-on (#:stumpwm #:clx-truetype)
  :components ((:file "package")
               (:file "ttf-fonts")))

