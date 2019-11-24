;;;; package.lisp

(defpackage #:command-history
  (:use
   :cl
   :stumpwm)
  (:export
   :*command-history-file*
   :*start-hook*
   :*quit-hook*))
