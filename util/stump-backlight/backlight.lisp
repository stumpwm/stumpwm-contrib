(in-package #:stump-backlight)

(defvar *scale* 10
  "The backlight scale. Increase if you want more granularity.")

(defvar *current-percent* 50
  "CLX does not let us query the existing backlight, so we need to keep track of it.")

(stumpwm:defcommand backlight-increase () ()
  (when (< *current-percent* 100)
    (setf *current-percent* (* (1+ (/ *current-percent* *scale*)) *scale*))
    (update)))

(stumpwm:defcommand backlight-decrease () ()
  (when (> *current-percent* 0)
    (setf *current-percent* (* (1- (/ *current-percent* *scale*)) *scale*))
    (update)))

(defun update ()
  (let* ((window (stumpwm:window-xwin (stumpwm:current-window)))
         (output (xlib:rr-get-output-primary window))
         (backlight-limits
           (multiple-value-list
            (xlib:rr-query-output-property stumpwm:*display* output :backlight)))
         (backlight-type (xlib:rr-get-output-property stumpwm:*display* output :backlight)))
    (destructuring-bind (min max) (fourth backlight-limits)
      (xlib:rr-change-output-property
       stumpwm:*display*
       output
       :backlight
       0 ; replace
       (vector (scaled-current (1+ min) (1- max))) ; max is non-inclusive in X
                                                   ; API, but inclusive in ours.
       backlight-type))))

(defun scaled-current (min max)
  (truncate (/ (* *current-percent* (- max min)) 100)))

;; Opinionated but this one should be fair.
(stumpwm:define-key stumpwm:*top-map*
    (stumpwm:kbd "XF86MonBrightnessUp")
  "backlight-increase")
(stumpwm:define-key stumpwm:*top-map*
    (stumpwm:kbd "XF86MonBrightnessDown")
  "backlight-decrease")
