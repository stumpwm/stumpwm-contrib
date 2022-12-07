(in-package :stump-volume-control)

(defvar *sound-card* 0
  "audio device to control by amixer; string, number, or null.
  If it is a string, it will be used as a device name as it is,
  if it is a number it will be translated to the string \"hw:0\",
  if NIL don't send any device at all.")

(defvar *pulse-audio-unmute-outputs* '("Speaker" "Headphone")
  "PulseAudio mutes more than it unmutes; when this variable is set,
  the listed audio outputs will explicitly be unmuted when toggling
  audio back on.

  \"Speaker\" and \"Headphone\" are the generic output names but
  some hardware might expose special names.

  Change to NIL if you just use ALSA and do not want the extra calls to
  be executed (which will fail silently if you do not have the outputs).")

(defun translate-device-to-option (device)
  "see docstring of *sound-card*"
  (etypecase device
    (string (format nil "-D ~a " device))
    (number (format nil "-D hw:~d " device))
    (null "")))

(defcommand volume-up () ()
  (run-shell-command (format nil "amixer ~asset Master playback 2db+"
                             (translate-device-to-option *sound-card*)))
  (message "Audio bit louder."))

(defcommand volume-down () ()
  (run-shell-command (format nil "amixer ~asset Master playback 2db-"
                             (translate-device-to-option *sound-card*)))
  (message "Audio bit quieter."))

(defcommand volume-toggle-mute () ()
  (let ((muted (search "[off]"
                       (run-shell-command
                        (format nil "amixer ~asset Master playback toggle"
                                (translate-device-to-option *sound-card*))
                        t))))
    (when (not muted)
      (dolist (output *pulse-audio-unmute-outputs*)
        ;; Just unmute all listed outputs explicitly when going back on.
        ;; Of course, we cannot fix PulseAudio with a small hack here.
        (run-shell-command (format nil "amixer ~asset ~s playback on"
                                   (translate-device-to-option *sound-card*)
                                   output))))
    (message (if muted
                 "Audio muted."
                 (format nil "Audio back on.~@[~%(Also switched on outputs: ~{~a~^, ~}.)~]"
                         *pulse-audio-unmute-outputs*)))))
