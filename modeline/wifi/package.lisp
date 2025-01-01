;;;; package.lisp

(defpackage #:wifi
  (:use #:cl :common-lisp :stumpwm )
  (:export #:*iwconfig-path*
           #:*wireless-device*
           #:*wifi-modeline-fmt*
           #:*wifi-signal-quality-fmt*
           #:*wifi-signal-quality-fmt-pc*
           #:*use-colors*))
