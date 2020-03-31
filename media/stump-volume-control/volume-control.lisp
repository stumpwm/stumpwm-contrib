(in-package :stump-volume-control)

(defvar *sound-card* 0
  "number of the sound card to control via amixer")

(defvar *pulse-audio-unmute-outputs* '("Speaker" "Headphone")
  "PulseAudio mutes more than it unmutes; when this variable is set,
  the listed audio outputs will explicitly be unmuted when toggling
  audio back on.

  \"Speaker\" and \"Headphone\" are the generic output names but
  some hardware might expose special names.

  Change to NIL if you just use ALSA and do not want the extra calls to
  be executed (which will fail silently if you do not have the outputs).")

(defcommand volume-up () ()
  (run-shell-command (format nil "amixer -c ~d sset Master playback 2db+" *sound-card*))
  (message "Audio bit lowder."))

(defcommand volume-down () ()
  (run-shell-command (format nil "amixer -c ~d sset Master playback 2db-" *sound-card*))
  (message "Audio bit quieter."))

(defcommand volume-toggle-mute () ()
  (let ((muted (search "[off]"
                       (run-shell-command
                        (format nil "amixer -c ~d sset Master playback toggle" *sound-card*)
                        t))))
    (when (not muted)
      (dolist (output *pulse-audio-unmute-outputs*)
        ;; Just unmute all listed outputs explicitly when going back on.
        ;; Of course, we cannot fix PulseAudio with a small hack here.
        (run-shell-command (format nil "amixer -c ~d sset ~s playback on"
                                   *sound-card*
                                   output))))
    (message (if muted
                 "Audio muted."
                 (format nil "Audio back on.~@[~%(Also switched on outputs: ~{~a~^, ~}.)~]"
                         *pulse-audio-unmute-outputs*)))))
