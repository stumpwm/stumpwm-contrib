(in-package :stump-volume-control)

(defcommand volume-up () ()
  (run-shell-command "amixer -c 0 sset Master playback 2db+")
  (message "Audio bit lowder."))

(defcommand volume-down () ()
  (run-shell-command "amixer -c 0 sset Master playback 2db-")
  (message "Audio bit quieter."))

(defcommand volume-toggle-mute () ()
  (let ((muted (search "[off]"
		       (run-shell-command "amixer -c 0 sset Master playback toggle" t))))
    (message (concatenate 'string "Audio " (if muted "muted" "back on") "."))))
