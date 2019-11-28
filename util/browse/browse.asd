(asdf:defsystem #:browse
  :description "Open the default web browser portably"
  :author "Spenser Truex <web@spensertruex.com>"
  :license  "GNU GPL v3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "browse"))
  :depends-on (#:stumpwm))
