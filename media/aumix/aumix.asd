;;;; aumix.asd

(asdf:defsystem #:aumix
  :serial t
  :description "Describe aumix here"
  :author "Fredrik Tolf"
  :license "GPLv3"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "aumix")))

