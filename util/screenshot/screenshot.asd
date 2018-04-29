;;;; screenshot.asd

(asdf:defsystem #:screenshot
  :serial t
  :description "Takes screenshots and stores them as png files"
  :author "Michael Filonenko"
  :license "GPLv3"
  :depends-on (#:zpng)
  :components ((:file "package")
               (:file "screenshot")))
