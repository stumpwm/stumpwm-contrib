;;;; swm-calibre.asd

(asdf:defsystem #:swm-gaps
  :description "Pretty (useless) gaps for StumpWM"
  :author "vlnx <https://github.com/vlnx>, Abhinav Tushar <abhinav.tushar.vs@gmail.com>"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "swm-gaps")))
