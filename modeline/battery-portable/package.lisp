;;;; package.lisp

(defpackage #:battery-portable
  (:use :common-lisp :stumpwm :cl-ppcre)
  (:export #:*refresh-time*
           #:*prefer-sysfs*
           ))


