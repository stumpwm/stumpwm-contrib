;;;; sdl-fonts.asd

(asdf:defsystem #:sdl-fonts
  :serial t
  :description "SDL-based TTF font rendering for StumpWM."
  :version "1.0.0"
  :author "Mihail Ivanchev"
  :license "MIT"
  :depends-on (#:stumpwm #:cffi #:cffi-libffi)
  :components ((:file "package")
               (:file "sdl-fonts")))

