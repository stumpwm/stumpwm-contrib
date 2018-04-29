;;;; perwindowlayout.asd

(asdf:defsystem #:perwindowlayout
  :serial t
  :description "Change the keyboard layout per window."
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm #:xkeyboard)
  :components ((:file "package")
               (:file "perwindowlayout")))
