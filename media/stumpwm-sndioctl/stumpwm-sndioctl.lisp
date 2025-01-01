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
(defvar *doas* nil "If t, this will use doas to run sndioctl.")

(defun call-sndioctl (args)
  "Calls sndioctl with the required arguments."
  (run-shell-command
   (format nil "~asndioctl ~a" (if *doas* "doas " "") args)
   t))

(defun get-mute-state-string ()
  "Returns a nicely formatted string containing the current mute state."
  (let* ((code-to-str
           (lambda (c)
             (cond
               ((string= "0" c) "No")
               ((string= "1" c) "Yes")
               (t "Err"))))
         (get-state
           (lambda (key)
             (handler-case
                 (funcall code-to-str
                          (aref (call-sndioctl (format nil "-n ~a" key)) 0))
               (error (c) (declare (ignore c)) "N/A"))
             ))
         (in-state (funcall get-state "input.mute"))
         (out-state (funcall get-state "output.mute")))
    (format nil "Muted in: ~a Muted out: ~a" in-state out-state)))

(defun get-volume-level-string ()
  "Returns a nicely formatted string containing the current volume level."
  (let*  ((get-volume
            (lambda (key)
              (handler-case
                  (format nil "~2$%"
                          (* 100
                             (with-input-from-string
                                 (vol-str (call-sndioctl (format nil "-n ~a" key)))
                               (read vol-str))))
                (error (c) (declare (ignore c)) "N/A"))))
          (in-volume (funcall get-volume "input.level"))
          (out-volume (funcall get-volume "output.level")))
    (format nil "Volume in: ~a Volume out: ~a" in-volume out-volume)))

(defun sndioctl-status-message ()
  "Returns a string indicating currrent sndioctl state."
  (format nil "~a ~a" (get-volume-level-string) (get-mute-state-string)))
       
(defcommand volume-up () ()
  "Volume goes up"
  (call-sndioctl (format nil "-q input.level=+~a" *step*))
  (call-sndioctl (format nil "-q output.level=+~a" *step*))
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand volume-down () ()
  "Volume goes down"
  (call-sndioctl (format nil "-q input.level=-~a" *step*))
  (call-sndioctl (format nil "-q output.level=-~a" *step*))
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand toggle-mute () ()
  "Toggles mute"
  (call-sndioctl "-n input.mute=!")
  (call-sndioctl "-n output.mute=!")
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand set-mute () ()
  "Force sets mute to ON"
  (call-sndioctl "-q input.mute=1")
  (call-sndioctl "-q output.mute=1")
  (if (not *terse*)
      (message (sndioctl-status-message))))

(defcommand unset-mute () ()
  "Force sets mute to OFF"
  (call-sndioctl "-q inout.mute=0")
  (call-sndioctl "-q output.mute=0")
  (if (not *terse*)
      (message (sndioctl-status-message))))
