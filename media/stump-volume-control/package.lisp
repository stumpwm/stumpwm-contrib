(in-package :common-lisp-user)

(defpackage #:stump-volume-control
  (:use #:cl #:stumpwm)
  (:export #:volume-up #:volume-down #:volume-toggle-mute
           #:*sound-card* #:*pulse-audio-unmute-outputs*))
