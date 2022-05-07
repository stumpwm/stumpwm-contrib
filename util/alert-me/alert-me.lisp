(in-package #:alert-me)


(defvar *alert-sound-file* #p"~/Music/bell.wav"
        "When set to a sound file pathname, plays on alert.")


(defvar *sound-play-command* "aplay"
  "System command to play sounds.")


(defvar notifications-loaded
  (when (member "notifications" (asdf:already-loaded-systems)
                :test #'string-equal) t)
  "True when StumpWM-Contrib `Notifications` module is loaded.")


(defun ring-alert ()
  "Play the *ALERT-SOUND-FILE* if exists."
  (when (probe-file *alert-sound-file*)
    (uiop:run-program
     (format nil "~a ~a" *sound-play-command* (namestring *alert-sound-file*)))))


(defun alert-message (alert-hour alert-minute message)
  (format nil "^[^3 ~2,'0d:~2,'0d ^2~a ^]"
          alert-hour alert-minute message))


(stumpwm:defcommand alert-me-at (alert-hour alert-minute message)
    ((:number "hour: ") (:number "minute: ") (:string "message: "))
  "Alert me at some point in time."
  (unless (validate-alert alert-hour alert-minute message)
    (error "One of the parameters is either empty or wrong."))
  (bordeaux-threads:make-thread
   #'(lambda ()
       (stumpwm:message "Alert is set: ~a"
                        (alert-message alert-hour alert-minute message))
       (loop
         do (sleep 5)
         when (multiple-value-bind (seconds minute hour)
                  (get-decoded-time)
                (declare (ignore seconds))
                (and (= alert-hour hour) (= alert-minute minute)))
           do (progn
                (when notifications-loaded
                  (notifications:notifications-add
                   (alert-message alert-hour alert-minute message)))
                (repeat-with-delay
                 10 3
                 #'(lambda (count)
                     (stumpwm:message "~a ^[^4[^1 ~d/3 ^4] ^]"
                                      (alert-message alert-hour alert-minute message)
                                      (1+ count))
                     (ring-alert)))
                (return))))
   :name (format nil "Alert thread: ~A" message)))


(defun validate-alert (alert-hour alert-minute message)
  (unless (or alert-hour alert-minute message)
    (return-from validate-alert nil))
  (multiple-value-bind (seconds minute hour)
      (get-decoded-time)
    (declare (ignore seconds))
    (when (or (> hour alert-hour)
              (and (= hour alert-hour)
                   (>= minute alert-minute)))
      (return-from validate-alert nil)))
  t)


(defun repeat-with-delay (delay times fn)
  (dotimes (i times)
    (apply fn `(,i))
    (sleep delay)))
