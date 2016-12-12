;;;; urgentwindows.asd

(asdf:defsystem #:urgentwindows
  :serial t
  :description "Allows focusing application windows that need user attention"
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "urgentwindows")))
