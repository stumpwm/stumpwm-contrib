;;;; cpu.asd

(asdf:defsystem #:cpu
  :serial t
  :description "Describe cpu here"
  :author "Anonymous Coward, Jonathan Moore Liles"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "cpu")))

