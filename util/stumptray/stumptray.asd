;;;; stumptray.asd
(asdf:compute-source-registry)
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(asdf:defsystem #:stumptray
  :serial t
  :description "System Tray for stumpwm."
  :author "Michael Filonenko"
  :license "GPLv3"
  :depends-on (#:stumpwm
               #:xembed
               #:alexandria)
  :components ((:file "package")
               (:file "stumptray")))

