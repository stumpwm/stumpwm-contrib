;;;; package.lisp

(defpackage #:urgentwindows
  (:use #:cl :stumpwm))

(in-package #:urgentwindows)

(import '(
  stumpwm::*urgent-window-hook*
  stumpwm::gselect
  stumpwm::message-no-timeout
  stumpwm::really-raise-window
  stumpwm::window-group
  stumpwm::window-title
  ))
