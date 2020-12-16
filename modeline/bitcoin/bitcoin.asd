;;;; bitcoin.asd

(asdf:defsystem "bitcoin"
  :description "Display bitcoin price on StumpWM modeline."
  :author "Santiago Payà Miralta @santiagopim"
  :license "GPLv3"
  :homepage "https://github.com/stumpwm/stumpwm-contrib/"
  :depends-on ("stumpwm"                ; Use add-screen-mode-line-formatter
               "dexador"                ; Get data from url
               "babel"                  ; Translate data to string
               "yason")                 ; Parse json
  :components ((:file "package")
               (:file "bitcoin" :depends-on ("package"))))
