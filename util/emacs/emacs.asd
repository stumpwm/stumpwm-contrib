;;;; emacs.asd

(asdf:defsystem #:emacs
  :serial t
  :description "Describe emacs here"
  :author "David Bjergaard, Alexander aka 'CosmonauT' Vynnyk"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "emacs")))

