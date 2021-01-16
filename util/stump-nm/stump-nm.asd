(defsystem "stump-nm"
  :serial t
  :description "StumpWM integration with NetworkManager"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on ("alexandria" "babel" "stumpwm" "dbus")
  :components ((:file "package")
               (:file "nm-api")
               (:file "stump-nm")))
