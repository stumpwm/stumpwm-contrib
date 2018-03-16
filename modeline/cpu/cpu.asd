;;;; cpu.asd

(asdf:defsystem #:cpu
  :serial t
  :description "Add cpu info to the modeline."
  :author "Anonymous Coward, Jonathan Moore Liles"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "cpu")))

