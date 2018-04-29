;;;; notifications.asd

(asdf:defsystem #:notifications
  :serial t
  :description "A notification library that sends notifications to the modeline via stumpish or from stumpwm itself."
  :author "Tassilo Horn <tassilo@member.fsf.org>"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "notifications")))

