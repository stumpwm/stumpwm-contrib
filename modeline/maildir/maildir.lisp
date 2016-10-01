;;;; maildir.lisp

(in-package #:maildir)

;;; "maildir" goes here. Hacks and glory await!

;;; Maildir monitoring for stumpwm's modeline
;;;
;;; Copyright 2007 Morgan Veyret.
;;;
;;; Maintainer: Morgan Veyret
;;;

;;; CODE:

(add-screen-mode-line-formatter #\M 'maildir-modeline)

(defvar *maildir-timer* nil)
(defvar *maildir-update-time* 900
  "Time between two updates of the maildir informations (in seconds).")

(defun maildir-set-update-time (time-in-seconds)
  "Set the maildir informations update interval."
  (when *maildir-timer*
    (cancel-timer *maildir-timer*))
  (setf *maildir-update-time* time-in-seconds)
  (setf *maildir-timer*
        (run-with-timer *maildir-update-time* *maildir-update-time*
                        'update-maildir-infos)))

(defvar *maildir-path*
  (merge-pathnames (make-pathname :directory '(:relative "Mail"))
                   (user-homedir-pathname))
  "Pathname to the mail directory. Defaults to ~/Mail.")

(defun maildir-mailboxes (maildir)
  "Returns a list of all mailboxes in *maildir-path*."
   (directory (merge-pathnames (make-pathname :directory '(:relative :wild))
                               maildir)))

(defun maildir-mailbox-dir (mailbox dir-name)
  "Returns the specified sub-directory pathname for the provided mailbox."
  (merge-pathnames (make-pathname :directory (list :relative dir-name)
                                  :name :wild :type :wild)
                   mailbox))


(defvar *maildir-new* '()
  "Number of new mails for each mailbox.")
(defvar *maildir-cur* '()
  "Number of mails for each mailbox.")
(defvar *maildir-tmp* '()
  "Number of tmp mails for each mailbox.")

(defun update-maildir-infos ()
  "Update mail counts for *maildir-path*."
  (loop for m in (maildir-mailboxes *maildir-path*)
     collect (length (directory (maildir-mailbox-dir m "new")))
     into nb-new
     collect (length (directory (maildir-mailbox-dir m "cur")))
     into nb-cur
     collect (length (directory (maildir-mailbox-dir m "tmp")))
     into nb-tmp
     finally (progn (setf *maildir-new* nb-new)
                    (setf *maildir-cur* nb-cur)
                    (setf *maildir-tmp* nb-tmp))))

;; modeline formatter
(defun maildir-modeline (ml)
  (declare (ignore ml))
  ;; setup a timer to check every *maildir-update-time* seconds
  ;; disk access are slow and you obviously don't need to check
  ;; emails every time the modeline gets updated
  (unless *maildir-timer*
    (update-maildir-infos)
    (setf *maildir-timer*
          (run-with-timer *maildir-update-time* *maildir-update-time*
                          'update-maildir-infos)))
  (format-expand *maildir-formatters-alist*
                 *maildir-modeline-fmt*))

(defun maildir-get-new ()
  (let ((total-new (reduce #'+ *maildir-new*)))
    (format nil "^[~A~D^]" (if (plusp total-new) "^B" "") total-new)))

(defun maildir-get-cur ()
  (let ((total-cur (reduce #'+ *maildir-cur*)))
    (format nil "~D" total-cur)))

(defun maildir-get-tmp ()
  (let ((total-tmp (reduce #'+ *maildir-tmp*)))
    (format nil "~D" total-tmp)))


(defvar *maildir-formatters-alist*
  '((#\n maildir-get-new)
    (#\c maildir-get-cur)
    (#\t maildir-get-tmp)))

(defvar *maildir-modeline-fmt* "%n %c"
  "The default value for displaying maildir information on the modeline.

@table @asis
@item %%
A literal '%'
@item %n
New mails number
@item %c
Current mails number
@item %t
Temporary mails number
@end table
")
