;;;; mpd.asd

(asdf:defsystem #:mpd
  :serial t
  :description "Displays information about the music player daemon (MPD)."
  :author "Morgan Veyret, Ivy Foster"
  :license "GPLv2"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "mpd")))

