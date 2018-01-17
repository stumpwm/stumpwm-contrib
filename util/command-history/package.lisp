;;;; package.lisp

(defpackage #:command-history
  (:use #:cl :stumpwm)
  (export '(*start-hook*
            *quit-hook*)))
