;;;; package.lisp

(defpackage #:windowtags
  (:use #:cl :stumpwm))

(in-package #:windowtags)

(import '(
  ; string wrappers for tag data storage
  stumpwm::string-to-utf8
  stumpwm::utf8-to-string
  ; switching windows
  stumpwm::really-raise-window
  ))
