;;;; package.lisp

(defpackage #:cpu
  (:use #:cl :stumpwm)
  (:export #:*cpu-modeline-fmt*
	   #:*acpi-thermal-zone*))

