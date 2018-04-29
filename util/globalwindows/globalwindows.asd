;;;; globalwindows.asd

(asdf:defsystem #:globalwindows
  :serial t
  :description "Manipulate all windows in the current X session"
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "globalwindows")))
