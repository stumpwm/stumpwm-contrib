(in-package #:stump-backlight)


(defvar *scale* 10
  "The backlight scale. Increase if you want more granularity.")

(defvar *default-percent* 50
  "Default value for an output's percentage.")

(defvar *use-clx-randr* t
  "Use CLX's randr extension.  If not, call shell command set by `*cli-format-str*'")

(defvar *cli-format-str* "brightnessctl s ~a%"
  "Format string that when passed an output percentage, sets the brightness.")

(defvar *current-percent* (make-hash-table)
  "CLX does not let us query the existing backlight, so we need to keep track of
  it manually.")

;; This just returns a random slot to hold in the hash table... It
;; doesn't return the actual output percentage.
(defun current-output ()
  (xlib:rr-get-output-primary (stumpwm:screen-root (stumpwm:current-screen))))


(defun cli-update (output)
  (stumpwm:dformat 5 (format nil *cli-format-str* output))
  (stumpwm:run-shell-command (format nil *cli-format-str* output)))

(stumpwm:defcommand backlight-increase () ()
  (let* ((output (current-output))
         (current-percent (or (gethash output *current-percent*) *default-percent*)))
    (when (< current-percent 100)
      (setf (gethash output *current-percent*) (* (1+ (/ current-percent *scale*)) *scale*))
      (if *use-clx-randr*
	  (update output)
	  (cli-update (gethash output *current-percent*))))))

(stumpwm:defcommand backlight-decrease () ()
  (let* ((output (current-output))
         (current-percent (or (gethash output *current-percent*) *default-percent*)))
    (when (> current-percent 0)
      (setf (gethash output *current-percent*) (* (1- (/ current-percent *scale*)) *scale*))
      (if *use-clx-randr*
	  (update output)
	  (cli-update (gethash output *current-percent*))))))


(defun update (output)
  (let ((backlight-limits
          (multiple-value-list
           (xlib:rr-query-output-property stumpwm:*display* output :backlight)))
        (backlight-type (xlib:rr-get-output-property stumpwm:*display* output :backlight)))
    (destructuring-bind (min max) (fourth backlight-limits)
      (xlib:rr-change-output-property
       stumpwm:*display*
       output
       :backlight
       0 ; replace
       (vector (scaled-current output (1+ min) (1- max))) ; max is non-inclusive
                                                          ; in X11 API, but
                                                          ; inclusive in ours.
       :atom-type backlight-type))))

(defun scaled-current (output min max)
  (truncate (/ (* (gethash output *current-percent*) (- max min)) 100)))

;; Opinionated but this one should be fair.
(stumpwm:define-key stumpwm:*top-map*
    (stumpwm:kbd "XF86MonBrightnessUp")
  "backlight-increase")
(stumpwm:define-key stumpwm:*top-map*
    (stumpwm:kbd "XF86MonBrightnessDown")
  "backlight-decrease")
