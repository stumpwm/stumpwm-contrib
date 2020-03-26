(asdf:defsystem #:stump-radio
  :description "mplayer-based radio for stumpvm"
  :author "Max-Gerd Retzlaff"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "radio" :depends-on ("package"))))
