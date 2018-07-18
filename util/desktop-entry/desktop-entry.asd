;;;; desktop-entry.asd

(asdf:defsystem #:desktop-entry
  :description "desktop-entry"
  :author "fqguozhou@gmail.com"
  :license "GPLv3"
  :depends-on (#:stumpwm #:py-configparser #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "desktop-entry")
               (:file "entry-list")
               (:file "desktop-menu")
               (:file "utils")))
