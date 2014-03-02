;;;; package.lisp

(defpackage #:stumptray
  (:use #:cl #:alexandria)
  (:export *tray-viwin-background*
           *tray-hiwin-background*

           add-mode-line-hooks
           remove-mode-line-hooks
           ))


