;;;; end-session.asd

(asdf:defsystem #:end-session
  :description "Provides commands to stumpwm that allow the user to shutdown, restart, and logoff through the stumpwm UI"
  :author "Stuart Dilts"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "end-session")))
