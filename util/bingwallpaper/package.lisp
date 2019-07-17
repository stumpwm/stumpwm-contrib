;;;; package.lisp

(defpackage #:bingwallpaper
  (:use #:cl #:alexandria #:local-time)
  (:import-from :cl-ppcre :regex-replace)
  (:export :bingwallpaper))
