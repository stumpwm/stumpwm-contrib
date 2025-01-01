(defsystem "stumpwm-mixer"
  :description "Interface to FreeBSD's built-in sound mixer"
  :author "Nyx <n1x@riseup.net>"
  :license "ISC"
  :serial t
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "mixer")))
