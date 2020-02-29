;;;; package.lisp

(defpackage :shell-command-history
  (:use :cl
        :stumpwm)
  (:export
   :*shell-command-history-file*
   :*start-hook*
   :*quit-hook*))
