;;;; package.lisp

(defpackage :bitcoin
  (:use :cl :stumpwm)   ; Using stumpwm:add-screen-mode-line-formatter
  (:export #:*modeline-use-colors*
           #:*last-values-size*))
