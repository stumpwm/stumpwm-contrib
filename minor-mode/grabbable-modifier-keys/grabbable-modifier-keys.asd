

(asdf:defsystem #:grabbable-modifier-keys
  :description "Allow StumpWM users to grab modifier keys and bind them"
  :license "GLPv3"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "gmk")))
