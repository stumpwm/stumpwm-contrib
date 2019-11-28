(in-package #:browse)
(defvar *homepage* "https://duckduckgo.com"
  "What page to open your browser to. Used as the argument to xdg-open.")
(defcommand browse () ()
  "Portably open the default browser with xdg."
  (run-shell-command (concatenate 'string "xdg-open '" *homepage* "'")))
