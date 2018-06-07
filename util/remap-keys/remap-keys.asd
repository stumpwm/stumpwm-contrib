;;;; remap-keys.asd

(asdf:defsystem :remap-keys
    :serial t
    :description "Remap keys for applications running inside StumpWM"
    :author "Ram Krishnan <kriyative@gmail.com>"
    :license "GPLv3"
    :depends-on (#:stumpwm)
    :components ((:file "package")
		 (:file "remap-keys")))
