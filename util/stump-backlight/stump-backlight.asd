(defsystem "stump-backlight"
  :serial t
  :description "Native backlight control from StumpWM"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :depends-on ("clx" "stumpwm")
  :components ((:file "package")
               (:file "backlight")))
