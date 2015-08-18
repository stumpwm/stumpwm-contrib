(in-package #:alert-me)

(stumpwm:defcommand alert-me-at (alert-hour alert-minute message) ((:number "hour: ") (:number "minute: ") (:string "message: "))
  "Alert me at some point in time."
  (unless (validate-alert alert-hour alert-minute message)
    (error "One of the parameters is either empty or wrong."))
  (bordeaux-threads:make-thread
   #'(lambda ()
       (loop
          do (sleep 5)
          when (multiple-value-bind (seconds minute hour)
                   (get-decoded-time)
                 (declare (ignore seconds))
                 (and (= alert-hour hour) (= alert-minute minute)))
          do (repeat-with-delay 10 3
               #'(lambda (count)
                   (stumpwm:message "The time for ~A is now! (~D/3 remainder)"
                                    message (1+ count))))))
   :name (format nil "Alert thread: ~A" message)))

(defun validate-alert (alert-hour alert-minute message)
  (unless (or alert-hour alert-minute message)
    (return-from validate-alert nil))
  (multiple-value-bind (seconds minute hour)
      (get-decoded-time)
    (declare (ignore seconds))
    (when (> hour alert-hour)
      (return-from validate-alert nil))
    (when (>= minute alert-minute)
      (return-from validate-alert nil)))
  t)

(defun repeat-with-delay (delay times fn)
  (dotimes (i times)
    (apply fn `(,i))
    (sleep delay)))
