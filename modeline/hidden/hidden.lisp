;;;; hidden.lisp

(in-package #:hidden)

;;; "hidden" goes here. Hacks and glory await!

;;; Hidden window formatter for the mode-line
;;;
;;; Copyright 2021 Woodrow Douglass 
;;;
;;; Maintainer: Woodrow Douglass
;;;

;; Install formatters.
(add-screen-mode-line-formatter #\H 'hidden-modeline)

(defun hidden-modeline (ml)
  (declare (ignore ml))
  (handler-case
      (progn
	(if (typep (stumpwm::current-group) 'stumpwm::tile-group)
	(let ((x (list-length (stumpwm::frame-windows (current-group) (stumpwm::current-frame)))))
    	  (if (>= x 1)
    	      (format NIL "(~A Hidden)" (- x 1))
    	      "(0 Hidden)"))
            ""))
    (t (c) (format nil "ERROR: ~A" c))))
  
