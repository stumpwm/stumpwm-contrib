;;;; pianobar.asd

(asdf:defsystem #:pianobar
  :serial t
  :description "Display Pandora song in modeline"
  :author "Ahmed Khanzada"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "pianobar")))
