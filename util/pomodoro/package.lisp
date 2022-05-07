;;;; package.lisp

(defpackage #:swm-pomodoro
  (:use #:cl #:stumpwm)
  (:export #:*bell-sound-file*
           #:*sound-play-command*))
(in-package :swm-pomodoro)


(defvar status-message nil
  "Status message for dashboard, as well as for StumpWM modeline
when `notifications` are on.")


(defvar notifications-loaded
  (when (member "notifications" (asdf:already-loaded-systems)
                :test #'string-equal) t)
  "True when StumpWM-Contrib `Notifications` module is loaded.")


(defvar *bell-sound-file* #p"~/Music/bell.wav"
  "When set to a sound file pathname, that sound will be played
at the end of each pomodoro.")


(defvar *sound-play-command* "aplay"
  "System command to play sounds.")


(defparameter *pomodoro-duration* 1500
  "Default duration of one pomodoro = 25 * 60 seconds.")


(defparameter *pomodoros-scored* 0
  "Counter of finished pomodoros in a series.")


(defparameter *pomodoro-timer* nil
  "The pomodoro timer clock object.")


(defun update-status-message ()
  (when notifications-loaded
    (notifications:notifications-delete status-message))
  (setf status-message (timer-status))
  (when notifications-loaded
    (notifications:notifications-add status-message)))


(defun finish-timer ()
  "Score Pomodoro."
  (sb-ext:unschedule-timer *pomodoro-timer*)
  (incf *pomodoros-scored*)
  (update-status-message)
  (stumpwm:message
   "+1 (`) Pomodoro Scored!~%~a" status-message)
  (ring-the-bell))


(setf *pomodoro-timer*
  (sb-ext:make-timer #'swm-pomodoro::finish-timer
                     :name :pomodoro-timer))


(defun reset-scored ()
  "Reset finished pomodoros series counter to 0."
  (setf *pomodoros-scored* 0)
  (update-status-message))


(defun ring-the-bell ()
  "Play the *BELL-SOUND-FILE* if exists."
  (when (probe-file *bell-sound-file*)
    (uiop:run-program
     (format nil "~a ~a" *sound-play-command* (namestring *bell-sound-file*)))))


(defun notify-break ()
  "Say what kind of break you deserve."
  (if (= *pomodoros-scored* 0)
      "^[^3Work harder!^]"
      (if (= 0 (mod *pomodoros-scored* 4))
          "^[^7^B[_])^2 15+ min!^]"
          "^[^2^B<-> 5 min^]")))


(defun many-scored ()
  "See how many have I scored."
  (format nil "^[^1^B(`)^6 ~a^]" *pomodoros-scored*))


(defun timer-status ()
  "Print the status of the timer."
  (if (sb-ext:timer-scheduled-p *pomodoro-timer*)
      "^[^1^B(`)^n in progress...^]"
      (format nil "~a : ~a"
              (many-scored) (notify-break))))


(stumpwm:defcommand pomodoro-status () ()
  (update-status-message)
  (stumpwm:message status-message))


(stumpwm:defcommand pomodoro-reset () ()
  "Reset global pomodoros series."
  (reset-scored)
  (stumpwm:message "Pomodoros series reset."))


(stumpwm:defcommand pomodoro-start-timer () ()
  (when (not (sb-ext:timer-scheduled-p *pomodoro-timer*))
    (sb-ext:schedule-timer *pomodoro-timer* *pomodoro-duration*)
    (stumpwm:message "Pomodoro timer set."))
  (update-status-message))


(stumpwm:defcommand pomodoro-cancel-timer () ()
  (sb-ext:unschedule-timer *pomodoro-timer*)
  (stumpwm:message "^1(7)^n Pomodoro timer cancelled!")
  (update-status-message))
