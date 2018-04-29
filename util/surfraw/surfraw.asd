;;;; surfraw.asd

(asdf:defsystem #:surfraw
  :serial t
  :description "Integrates surfraw with stumpwm."
  :author "Ivy Foster"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "surfraw" :depends-on ("package"))
               (:file "macros" :depends-on ("surfraw"))))

