;;;; package.lisp

(defpackage #:app-menu
  (:use #:cl :stumpwm)
  (:export :*app-menu*
           :show-menu
           :load-menu-file))
