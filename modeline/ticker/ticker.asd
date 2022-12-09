;;;; bitcoin.asd

(asdf:defsystem "ticker"
  :description "Display ticker price on StumpWM modeline."
  :author "Santiago Payà Miralta @santiagopim"
  :license "MIT"
  :homepage "https://github.com/stumpwm/stumpwm-contrib/"
  :depends-on ("stumpwm"                ; Use add-screen-mode-line-formatter
               "dexador"                ; Get data from url
               "yason")                 ; Parse json
  :components ((:file "package")
               (:file "ticker" :depends-on ("package"))))
