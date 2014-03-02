;;;; windowtags.asd

(asdf:defsystem #:windowtags
  :serial t
  :description "Describe windowtags here"
  :author "Michael Raskin"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "windowtags")))

