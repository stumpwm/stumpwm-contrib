;;;; swm-pomodoro.asd

(asdf:defsystem #:swm-pomodoro
  :description "Pretty basic Pomodoro-tracker for StumpWM."
  :author "Vladimir (Hawthorne) <vdikan@vivaldi.net>"
  :license  "GPLv3"
  :version "0.2.0"
  :serial t
  :depends-on (#:uiop #:stumpwm #:notifications)
  :components ((:file "package")
               (:file "swm-pomodoro")))
