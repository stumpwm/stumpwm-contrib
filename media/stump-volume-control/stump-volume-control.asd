(asdf:defsystem #:stump-volume-control
  :description "Minimalistic amixer-based volume control for StumpWM."
  :author "Max-Gerd Retzlaff"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "volume-control" :depends-on ("package"))))
