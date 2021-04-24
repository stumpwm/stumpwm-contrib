;;;; stumpwm-sndioctl.lisp

;;;; Copyright 2021, Dr Ashton Fagg <ashton@fagg.id.au>
;;;;
;;;; Permission to use, copy, modify, and/or distribute this software for
;;;; any purpose with or without fee is hereby granted, provided that the
;;;; above copyright notice and this permission notice appear in all
;;;; copies.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;;;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;;; PERFORMANCE OF THIS SOFTWARE.


(in-package #:stumpwm-sndioctl)


(defvar *step* 0.05)
(defvar *terse* nil "If t, this will supress messages that get shown on changes to volume.")

(defun call-sndioctl (args captive)
  "Calls sndioctl with the required arguments."
  (run-shell-command (format nil "sndioctl ~a" args) captive))

(defun make-step-cmd (direction)
  "Helpful wrapper for generating command strings for incrementing/decrementing volume."
  (cond 
    ((string= "+" direction)
     (format nil "-q output.level=+~a" *step*))
    ((string= "-" direction) 
     (format nil "-q output.level=-~a" *step*))
    (t (error "Not a valid step direction!"))))

(defun get-mute-state-string ()
  "Returns a nicely formatted string containing the current mute state."
  (let ((state (aref (call-sndioctl "-n output.mute" t) 0)))
    (cond 
      ((string= "0" state)
       (format nil "Muted: No"))
      ((string= "1" state)
       (format nil "Muted: Yes"))
      (t (error "Unknown output from sndioctl -n output.mute")))))

(defun get-volume-level-string ()
  "Returns a nicely formatted string containing the current volume level."
  (let ((level
	  (with-input-from-string (vol-str (call-sndioctl "-n output.level" t))
	    (read vol-str))))
    (format nil "Volume: ~2$%" (* 100.0 level))))

(defun sndioctl-status-message ()
  "Returns a string indicating currrent sndioctl state."
  (format nil "~a ~a" (get-volume-level-string) (get-mute-state-string)))
       
(defcommand volume-up () ()
  "Volume goes up"
  (call-sndioctl (make-step-cmd "+") nil)
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand volume-down () ()
  "Volume goes down"
  (call-sndioctl (make-step-cmd "-") nil)
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand toggle-mute () ()
  "Toggles mute"
  (call-sndioctl "-q output.mute=!" nil)
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand set-mute () ()
  "Force sets mute to ON"
  (call-sndioctl "-q output.mute=1" nil)
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand unset-mute () ()
  "Force sets mute to OFF"
  (call-sndioctl "-q output.mute=0" nil)
  (if (not *terse*)
      (message (sndioctl-status-message))))
