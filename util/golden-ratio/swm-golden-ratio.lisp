(in-package #:swm-golden-ratio)

(export '(*golden-ratio* *golden-ratio-on* toggle-golden-ratio))

(defvar *golden-ratio* 0.6180)
(defparameter *golden-ratio-on* nil)

(defun target-px (size-px)
  (floor (*  size-px *golden-ratio*)))

(defun resize-px (target current-px)
  (- target current-px))

(defun resize-to-golden-ratio (to-frame from-frame)
  (when (and *golden-ratio-on* (not (stumpwm::single-frame-p)))
    (let* ((target-x (target-px (stumpwm::head-width (stumpwm:current-head))))
           (target-y (target-px (stumpwm::head-height (stumpwm:current-head)))))
      (setq *golden-ratio-on* nil)
      (stumpwm:balance-frames)
      (stumpwm:resize (resize-px target-x
                                 (stumpwm::frame-width to-frame))
                      (resize-px target-y
                                 (stumpwm::frame-height to-frame)))
      (setq *golden-ratio-on* t))))

(stumpwm:defcommand toggle-golden-ratio () ()
  "Toggle golden ratio"
  (setf *golden-ratio-on* (null *golden-ratio-on*)))

(stumpwm:add-hook stumpwm:*focus-frame-hook* 'resize-to-golden-ratio)
