;;;; package.lisp

(defpackage #:wifi
  (:use #:cl :common-lisp :stumpwm )
  (:export #:*iwconfig-path*
           #:*wireless-device*
           #:*wifi-modeline-fmt*
           #:*use-colors*))

