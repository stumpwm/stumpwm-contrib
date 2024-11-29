;;;; surfraw.asd

(asdf:defsystem #:surfraw
  :serial t
  :description "Integrates surfraw with stumpwm."
  :author "Ivy Foster"
  :license "GPLv3"
  :depends-on (#:stumpwm #:alexandria #:uiop)
  :components ((:file "package")
               (:file "surfraw")
               (:file "auto-commands")))
