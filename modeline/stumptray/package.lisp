;;;; package.lisp

(defpackage #:stumptray
  (:use #:cl #:alexandria)
  (:export *tray-viwin-background*
           *tray-hiwin-background*
           *tray-placeholder-pixels-per-space*
	   *tray-icon-spacing*

           add-mode-line-hooks
           remove-mode-line-hooks
           ))


