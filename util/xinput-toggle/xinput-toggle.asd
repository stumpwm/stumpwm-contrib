;;;; xinput-toggle.asd

(asdf:defsystem #:xinput-toggle
  :serial t
  :description "Toggle (on/off) devices such as the touchpad, mouse, etc."
  :author "Kayomarz Gazder <kayomarz@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm #:cl-ppcre)
  :components ((:file "package")
               (:file "xinput-toggle")))
