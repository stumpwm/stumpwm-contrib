;;;; bitcoin.asd

(asdf:defsystem "bitcoin"
  :description "Display bitcoin price on StumpWM modeline."
  :author "Santiago Payà Miralta @santiagopim"
  :license "GPLv3"
  :homepage "https://github.com/stumpwm/stumpwm-contrib/"
  :depends-on ("stumpwm"                ; Use add-screen-mode-line-formatter
               "lparallel"              ; Connect to API with concurrency
               "dexador"                ; Get data from url
               "yason")                 ; Parse json
  :components ((:file "package")
               (:file "bitcoin" :depends-on ("package"))))
