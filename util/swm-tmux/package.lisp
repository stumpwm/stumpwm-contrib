;;;; package.lisp

(defpackage #:swm-tmux
  (:use #:cl #:stumpwm)
  (:export #:*swm-tmux-default-term*
           #:*swm-tmux-default-term-title-opt*
           #:*swm-tmux-default-term-exec-opt*))
