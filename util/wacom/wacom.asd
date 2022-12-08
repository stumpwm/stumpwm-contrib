;;;; wacom.asd

(asdf:defsystem #:wacom
  :description "Map StumpWM frames to Wacom tablets using `xsetwacom`."
  :author "Eric Ihli <eihli@owoga.com>"
  :license  "GPLv3"
  :version "1.0.0"
  :serial t
  :depends-on (#:stumpwm #:cl-ppcre)
  :components ((:file "package")
               (:file "wacom")))
