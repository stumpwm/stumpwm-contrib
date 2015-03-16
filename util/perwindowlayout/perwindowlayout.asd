;;;; perwindowlayout.asd

(asdf:defsystem #:perwindowlayout
  :serial t
  :description "Describe perwindowlayout here"
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm #:xkeyboard)
  :components ((:file "package")
               (:file "perwindowlayout")))
