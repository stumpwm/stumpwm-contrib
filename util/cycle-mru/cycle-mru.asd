;;;; cycle-mru.asd

(asdf:defsystem #:cycle-mru
  :description "Cycle windows in most recently used order."
  :author "Toby Slight"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
	       (:file "cycle-mru")))
