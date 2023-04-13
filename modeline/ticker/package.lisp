;;;; package.lisp

(defpackage :ticker
  (:use :cl)
  (:export #:define-ticker
           #:*tickers-separator*))
