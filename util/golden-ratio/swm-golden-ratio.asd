;;;; swm-golden-ratio.asd

(asdf:defsystem #:swm-golden-ratio
  :description "Resize the currently focused frame to the golden ratio"
  :author "Roch D'Amour <roch.damour@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "swm-golden-ratio")))
