;;;; notifications.asd

(asdf:defsystem #:notifications
  :serial t
  :description "Describe notifications here"
  :author "Tassilo Horn <tassilo@member.fsf.org>"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "notifications")))

