(in-package :stump-volume-control)

(defvar *sound-card* 0
  "number of the sound card to control via amixer")

(defvar *pulse-audio-unmute-hack* nil
  "PulseAudio mutes more than it unmutes; this hack will also unmute the outputs
  (See comment in VOLUME-TOGGLE-MUTE for more details.)")

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
    (when (and (not muted)
               *pulse-audio-unmute-hack*)
      ;; hack for pulseaudio:
      ;;   On my system with PulseAudio muting 'Master' also mutes the
      ;;   output 'Speaker' (or 'Headphone') but when unmuting 'Master'
      ;;   the output 'Speaker (or 'Headphone') stays muted, so we just
      ;;   explicitly unmute both seperately. This seems to work well.
      ;;
      ;;   As noted, this is a hack. It might work for you or it might not.
      ;;   Of course, we cannot fix PulseAudio with a small hack here.
      (run-shell-command (format nil "amixer -c ~d sset Speaker playback on" *sound-card*))
      (run-shell-command (format nil "amixer -c ~d sset Headphone playback on" *sound-card*)))
    (message (concatenate 'string "Audio " (if muted "muted" "back on") "."))))
