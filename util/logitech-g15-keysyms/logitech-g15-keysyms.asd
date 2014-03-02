;;;; logitech-g15-keysyms.asd

(asdf:defsystem #:logitech-g15-keysyms
  :serial t
  :description "Describe logitech-g15-keysyms here"
  :author "Ted Zlatanov"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "logitech-g15-keysyms")))

