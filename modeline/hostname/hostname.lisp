;; hostname.lisp
;;
;; Put %h in your modeline format string to show your hostname
;;

(defpackage #:hostname
  (:use #:cl :stumpwm))

(in-package #:hostname)

(defun fmt-hostname (ml)
  "Return hostname"
  (declare (ignore ml))
  (format nil "~a" (car (split-string (machine-instance) ". "))))

;; Install formatter
(add-screen-mode-line-formatter #\h #'fmt-hostname)