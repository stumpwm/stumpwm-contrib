;;;; emacs.asd

(asdf:defsystem #:swm-emacs
  :serial t
  :description "A set of utilities for launching the beast."
  :author "David Bjergaard, Alexander aka 'CosmonauT' Vynnyk"
  :license "GPLv3"
  :depends-on (#:stumpwm #:cl-fad)
  :components ((:file "package")
               (:file "swm-emacs")))
