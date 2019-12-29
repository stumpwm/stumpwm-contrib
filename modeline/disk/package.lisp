;;;; package.lisp

(defpackage #:disk
  (:use #:cl
        #:stumpwm)
  (:export
   #:*disk-modeline-fmt*
   #:*disk-usage-paths*))
