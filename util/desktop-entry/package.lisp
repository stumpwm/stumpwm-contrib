;;;; package.lisp

(defpackage #:desktop-entry
  (:use #:cl)
  (:export :show-menu
           :load-desktop-file
           :add-favorite-entry
           :init-entry-list
           :add-to-entry-list
           :desktop-entry
           :make-desktop-entry
           :*entry-list*
           :*favorite-list*
           :*entry-paths*))
