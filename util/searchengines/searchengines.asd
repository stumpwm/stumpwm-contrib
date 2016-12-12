;;;; searchengines.asd

(asdf:defsystem #:searchengines
  :serial t
  :description "Allows searching text using prompt or clipboard contents with various search engines"
  :author "Alex Ermolov"
  :license "GPLv3"
  :depends-on (#:stumpwm #:drakma)
  :components ((:file "package")
               (:file "searchengines")))
