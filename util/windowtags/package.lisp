;;;; package.lisp

(defpackage #:windowtags
  (:use #:cl #:stumpwm)
  (:import-from #:stumpwm
                ;; string wrappers for tag data storage
                #:utf8-to-string
                ;; groups
                #:find-group
                ;; switching windows
                #:really-raise-window))
