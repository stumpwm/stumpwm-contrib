;;;; globalwindows.asd

(asdf:defsystem #:globalwindows
  :serial t
  :description "Describe globalwindows here"
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "globalwindows")))
