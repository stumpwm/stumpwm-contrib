;;;; swm-tmux.asd

(asdf:defsystem "swm-tmux"
  :description "Tmux session manager for StumpWM"
  :author "Kirill A. Korinsky <kirill@korins.ky>"
  :license "GPLv3"
  :version "0.1"
  :serial t
  :depends-on ("stumpwm" "cl-ppcre")
  :components ((:file "package")
               (:file "swm-tmux")))
