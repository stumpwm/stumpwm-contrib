(asdf:defsystem #:alert-me
  :serial t
  :description "Alert me that an event is coming"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on (:bordeaux-threads :stumpwm)
  :components ((:file "package")
               (:file "alert-me")))
