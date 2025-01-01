;;;; package.lisp

(defpackage #:surfraw
  (:use #:cl #:stumpwm)
  (:import-from #:uiop
                #:run-program
                #:file-exists-p
                #:read-file-string)
  (:import-from #:alexandria
                #:if-let
                #:format-symbol))
