;;;; sensors.asd

(asdf:defsystem #:sensors
  :description "View CPU temperature and fan RPM in modeline"
  :author "Toby Slight <tslight@pm.me>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
	       (:file "sensors")))
