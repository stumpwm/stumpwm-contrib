;;;; package.lisp

(defpackage #:stumptray
  (:use #:cl #:alexandria)
  (:export *tray-viwin-background*
           *tray-hiwin-background*
           *tray-placeholder-pixels-per-space*

           add-mode-line-hooks
           remove-mode-line-hooks
           ))


