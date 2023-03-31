;;;; package.lisp

(defpackage :bitcoin
  (:use :cl)
  (:export #:*modeline-use-colors*
           #:*threshold*
           #:*time-delay*
           #:*local-code*
           #:*decimals*
           #:*modeline-gauge*
           #:*gauge-width*))
