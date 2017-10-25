;;;; package.lisp

(defpackage #:searchengines
  (:use #:cl :stumpwm :drakma)
  (:import-from #:stumpwm #:message-no-timeout)
  (:export #:*search-browser-executable*
           #:*search-browser-params*
           #:make-searchengine-prompt
           #:make-searchengine-selection
           #:make-searchengine-augmented))

(in-package #:searchengines)
