;;;; qubes.asd

(asdf:defsystem #:qubes
  :serial t
  :description "Integration to Qubes OS (https://www.qubes-os.org)"
  :author "Johanna Abrahamsson"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "qubes")))

