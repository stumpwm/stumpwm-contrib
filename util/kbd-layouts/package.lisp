;;;; package.lisp

(defpackage #:kbd-layouts
  (:use #:cl #:stumpwm)
  (:export *caps-lock-behavior*
           *custom-setxkb-options*
           *run-xmodmap*
           keyboard-layout-list))


