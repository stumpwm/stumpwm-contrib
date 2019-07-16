(asdf:defsystem "gnu-pw-mgr"
  :description "Reconstruct passwords with gnu-pw-mgr"
  :author "Brandon Invergo <brandon@invergo.net>"
  :license  "GPLv3"
  :version "0.1"
  :serial t
  :depends-on ("stumpwm"
               "cl-ppcre")
  :components ((:file "package")
               (:file "gnu-pw-mgr")))
