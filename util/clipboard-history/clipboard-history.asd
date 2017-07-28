;;;; clipboard-history.asd

(asdf:defsystem :clipboard-history
    :serial t
    :description "Simple clipboard history module for StumpWM"
    :author "Ram Krishnan <kriyative@gmail.com>"
    :license "GPLv3"
    :depends-on (#:stumpwm)
    :components ((:file "package")
		 (:file "clipboard-history")))
