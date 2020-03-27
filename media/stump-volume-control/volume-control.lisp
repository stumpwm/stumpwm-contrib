(in-package :stump-volume-control)

(defvar *sound-card* 0
  "number of the sound card to control via amixer")

(defcommand volume-up () ()
  (run-shell-command (format nil "amixer -c ~d sset Master playback 2db+" *sound-card*))
  (message "Audio bit lowder."))

(defcommand volume-down () ()
  (run-shell-command (format nil "amixer -c ~d sset Master playback 2db-" *sound-card*))
  (message "Audio bit quieter."))

(defcommand volume-toggle-mute () ()
  (let ((muted (search "[off]"
                       (run-shell-command (format nil "amixer -c ~d sset Master playback toggle" *sound-card*)
                                          t))))
    (unless muted
      ;; hack for pulseaudio: on my system with PulseAudio muting Master also mutes Speaker
      ;; but when unmuting Master Speaker stays muted, so unmuting it seperately.
      ;; (On my system with just ALSA there is no Speaker so this will fail, but just silently...)
      (run-shell-command (format nil "amixer -c ~d sset Speaker playback toggle" *sound-card*)))
    (message (concatenate 'string "Audio " (if muted "muted" "back on") "."))))
