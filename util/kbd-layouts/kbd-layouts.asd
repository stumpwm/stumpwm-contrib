;;;; kbd-layouts.asd
(asdf:compute-source-registry)
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(asdf:defsystem #:kbd-layouts
  :serial t
  :description "Keyboard layout switcher for StumpWM"
  :author "Wojciech Gac"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "kbd-layouts")))

