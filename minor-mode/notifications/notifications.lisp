;;;; notifications.lisp -- Poor man's systray for StumpWM
(in-package #:notifications)
;; Copyright 2008 Tassilo Horn <tassilo@member.fsf.org>
;;
;; Maintainer: 
;;

;;; Code:
(add-screen-mode-line-formatter #\N 'notifications-as-string)

(defparameter *notifications-delimiters* '("[" "]"))

(defvar notifications nil
  "A list of notification strings.")

(defcommand notifications-add (str)
  ((:rest "Notification: "))
  "Add a notification string.
If a notification is already included, it will be moved to the front instead of
added anew."
  (when (not (string= (car notifications) str))
    (when (member str notifications :test #'string=)
      (setf notifications (delete str notifications :test #'string=)))
    (push str notifications)))

(defcommand notifications-reset ()
  ()
  "Clear all notifications."
  (setf notifications nil))

(defcommand notifications-delete (str)
  ((:rest "Notification: "))
  "Delete the specified notification."
  (setf notifications (delete str notifications :test #'string=)))

(defcommand notifications-delete-first ()
  ()
  "Delete the first notification."
  (setf notifications (cdr notifications)))

(defcommand notifications-delete-last ()
  ()
  "Delete the first notification."
  (setf notifications (nreverse (cdr (nreverse notifications)))))

(defun notifications-as-string (&rest r)
  (declare (ignore r))
  (if notifications
      (format nil "~a ~{ ~a ~#[~:;;~]~} ~a"
              (first *notifications-delimiters*)
              notifications
              (second *notifications-delimiters*))
    ""))

(defcommand notifications-show ()
  ()
  "Messages all notifications."
  (message "Notifications: ~a" (notifications-as-string)))

(defvar *notifications-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a")     "notifications-add")
    (define-key m (kbd "r")     "notifications-reset")
    (define-key m (kbd "d")     "notifications-delete-first")
    (define-key m (kbd "D")     "notifications-delete-last")
    (define-key m (kbd "s")     "notifications-show")
    m))

;; End:
