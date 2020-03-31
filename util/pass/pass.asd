(asdf:defsystem #:pass
    :serial t
    :description "Integrate 'pass' with StumpWM"
    :author "Florian Margaine <florian@margaine.com>"
    :license "GPLv3"
    :depends-on (:stumpwm :uiop)
    :components ((:file "pass")))
