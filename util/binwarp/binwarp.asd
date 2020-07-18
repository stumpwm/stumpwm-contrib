;;;; binwarp.asd

(asdf:defsystem #:binwarp
  :description "Keyboard-driven divide-and-conquer mouse control mode."
  :author "Artyom Bologov"
  :license "GPLv3"
  :version "0.9"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "binwarp")))
