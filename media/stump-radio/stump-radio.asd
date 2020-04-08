(asdf:defsystem #:stump-radio
  :description "Minimalistic mplayer-based radio for StumpWM."
  :author "Max-Gerd Retzlaff"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "radio" :depends-on ("package"))))
