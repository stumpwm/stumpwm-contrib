;;;; pianobar.asd

(asdf:defsystem #:pianobar
  :serial t
  :description "Display Pianobar's now playing info in modeline"
  :author "Ahmed Khanzada"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "pianobar")))
