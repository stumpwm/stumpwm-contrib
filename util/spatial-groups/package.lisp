;;;; package.lisp

(defpackage #:spatial-groups
  (:use #:cl #:stumpwm)
  (:export *spatial-banish-on-move*
    spatial-gselect
    install-default-keybinds))
