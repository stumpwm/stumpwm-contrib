;;;; aumix.asd

(asdf:defsystem #:aumix
  :serial t
  :description "Manipulate your volume with aumix"
  :author "Fredrik Tolf"
  :license "GPLv3"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "aumix")))

