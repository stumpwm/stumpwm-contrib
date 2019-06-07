;;;; package.lisp

(defpackage #:sensors
  (:use #:cl #:stumpwm #:cl-ppcre)
  (:export #:*refresh-time*
	   #:*red-above-temp*
	   #:*yellow-above-temp*
	   #:*display-above-temp*
	   #:*red-above-rpm*
	   #:*yellow-above-rpm*
	   #:*display-above-rpm*
	   #:*ignore-below*))
