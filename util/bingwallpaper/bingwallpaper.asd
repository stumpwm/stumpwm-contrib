;;;; bingwallpaper.asd

(asdf:defsystem #:bingwallpaper
  :description "Sets the daily Bing wallpaper using feh"
  :author "brabes"
  :license  "CC0"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:cl-json #:alexandria #:local-time #:cl-ppcre #:stumpwm)
  :components ((:file "package")
               (:file "bingwallpaper")))
