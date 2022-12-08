(in-package #:urgentwindows)

(export '(
  raise-urgent
  *urgent-window-message*))

(defvar *urgent-windows-stack* nil
  "Stack of windows gone urgent. After activating a window from the stack,
 it goes off the stack")

(defvar *urgent-window-message* "~a needs your attention."
  "Message template to be displayed to grab user's attention")

(defun echo-urgent-window (target)
  (message-no-timeout *urgent-window-message* (window-title target))
  (push target *urgent-windows-stack*))

(add-hook *urgent-window-hook* 'echo-urgent-window)

(defun raise-urgent-window ()
  (let ((last-urgent (pop *urgent-windows-stack*)))
    (when last-urgent
      (gselect (group-name (window-group last-urgent)))
      (really-raise-window last-urgent))))

(defcommand raise-urgent () ()
  "Raise urgent window"
  (raise-urgent-window))
