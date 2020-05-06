;;;; bitcoin.asd

(asdf:defsystem "bitcoin"
  :serial t
  :description "Display bitcoin price on StumpWM modeline."
  :author "Santiago Payà Miralta @santiagopim"
  :license "GPLv3"
  :depends-on ("stumpwm"
               "dexador"                ; Get data from url
               "babel"                  ; Translate data to string
               "yason")                 ; Parse json
  :components ((:file "package")
               (:file "bitcoin")))
