;;;; screenshot.asd

(asdf:defsystem #:screenshot
  :serial t
  :description "Describe screenshot here"
  :author "Michael Filonenko"
  :license "GPLv3"
  :depends-on (#:zpng)
  :components ((:file "package")
               (:file "screenshot")))
