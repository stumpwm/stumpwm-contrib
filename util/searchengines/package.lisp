;;;; package.lisp

(defpackage #:searchengines
  (:use #:cl :stumpwm :drakma)
  (:export #:*search-browser-executable*
           #:*search-browser-params*
           #:make-searchengine-prompt
           #:make-searchengine-selection
           #:make-searchengine-augmented))

(in-package #:searchengines)

(import '(
          drakma:url-encode
          stumpwm::defcommand
          stumpwm::get-x-selection
          stumpwm::message-no-timeout
          stumpwm::run-shell-command
          ))
