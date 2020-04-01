;;;; package.lisp

(defpackage #:amixer
  (:use #:cl :stumpwm)
  (:import-from :alexandria :with-gensyms)
  (:export
   #:*default-device*))

