(asdf:defsystem #:stump-volume-control
  :description "very simple volume control for stumpvm"
  :author "Max-Gerd Retzlaff"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "volume-control" :depends-on ("package"))))
