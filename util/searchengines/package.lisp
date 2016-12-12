;;;; package.lisp

(defpackage #:searchengines
  (:use #:cl :stumpwm :drakma))

(in-package #:searchengines)

(import '(
  drakma:url-encode
  stumpwm::defcommand
  stumpwm::get-x-selection
  stumpwm::message-no-timeout
  stumpwm::run-shell-command
  ))
