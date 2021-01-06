;;;; lookup.asd

(asdf:defsystem #:lookup
  :serial t
  :description "Dictionary/search engine lookup module for StumpWM."
  :author "Wojciech S. Gac <wojciech.s.gac@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm #:alexandria #:quri)
  :components ((:file "package")
               (:file "lookup")))
