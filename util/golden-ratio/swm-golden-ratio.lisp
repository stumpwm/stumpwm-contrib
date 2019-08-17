(in-package #:swm-golden-ratio)

(export '(*golden-ratio* *golden-ratio-on* toggle-golden-ratio))

(defvar *golden-ratio* 0.6180)
(defparameter *golden-ratio-on* nil)

(defun target-px (size-px)
  (floor (*  size-px *golden-ratio*)))

(defun resize-px (target current-px)
  (- target current-px))

(defun balance-current-group ()
  (stumpwm::balance-frames-internal (stumpwm:current-group)
                                    (list (stumpwm::tile-group-frame-head
                                           (stumpwm:current-group)
                                           (stumpwm:current-head)))))

(defun resize-to-golden-ratio (to-frame from-frame)
  (if *golden-ratio-on*
      (let* ((target-x (target-px (stumpwm::head-width (stumpwm:current-head))))
             (target-y (target-px (stumpwm::head-height (stumpwm:current-head)))))
        (if *golden-ratio-on*
            (progn (setq *golden-ratio-on* nil)
                   (balance-current-group)
                   (stumpwm:resize (resize-px target-x
                                              (stumpwm::frame-width to-frame))
                                   (resize-px target-y
                                              (stumpwm::frame-height to-frame)))
                   (stumpwm:message "resized")
                   (setq *golden-ratio-on* T))))))

  (stumpwm:defcommand toggle-golden-ratio () ()
    "Toggle golden ratio"
    (setf *golden-ratio-on* (null *golden-ratio-on*)))

(stumpwm:message "reloaded")

(stumpwm:add-hook stumpwm:*focus-frame-hook* 'resize-to-golden-ratio)
