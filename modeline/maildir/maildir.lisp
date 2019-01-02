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

(export '(*maildir-alist*
		  *maildir-modeline-fmt*
		  *maildir-update-time*))



(defvar *maildir-timer* nil)

(defvar *maildir-update-time* 900
  "Time between two updates of the maildir informations (in seconds).")

(defvar *maildir-info* '()
  "List of plists for number of {new,cur,tmp} mail for each mailbox")

(defvar *maildir-alist*
  (list (cons "Mail" (merge-pathnames (make-pathname :directory '(:relative "Mail"))
									  (user-homedir-pathname))))
  "Alist of pathnames to the mail directories with names. Defaults to just ~/Mail.")

(defvar *maildir-modeline-fmt* "%l: %n "
  "The Default Value For Displaying Maildir information on the modeline.

@table @asis
@item %%
A literal '%'
@item %l
Label of the maildir
@item %n
New mails number
@item %c
Current mails number
@item %t
Temporary mails number
@end table")



(defun maildir-mailboxes (maildir)
  "Returns a list of all mailboxes in *maildir-path*."
  (cons maildir (directory (merge-pathnames (make-pathname :directory
														   '(:relative :wild))
											maildir))))

(defun maildir-mailbox-dir (mailbox dir-name)
  "Returns the specified sub-directory pathname for the provided mailbox."
  (merge-pathnames (make-pathname :directory (list :relative dir-name)
								  :name :wild :type :wild)
				   mailbox))

(defun update-maildir-infos ()
  "Update mail counts for *maildir-alist*."
  (setf *maildir-info* '())
  (loop for (label . dir) in *maildir-alist*
		when (probe-file dir)
		  do (loop for m in (maildir-mailboxes dir)
				   sum (length (directory (maildir-mailbox-dir m "new"))) into nb-new
				   sum (length (directory (maildir-mailbox-dir m "cur"))) into nb-cur
				   sum (length (directory (maildir-mailbox-dir m "tmp"))) into nb-tmp
				   finally (push (list label :new nb-new :cur nb-cur :tmp nb-tmp)
								 *maildir-info*))))

;; modeline formatter
(defun maildir-modeline (ml)
  (declare (ignore ml))
  ;; setup a timer to check every *maildir-update-time* seconds
  ;; disk access are slow and you obviously don't need to check
  ;; emails every time the modeline gets updated
  (unless *maildir-timer*
	(setf *maildir-timer*
		  (run-with-timer 0 *maildir-update-time* #'update-maildir-infos)))
  (loop for (label . info) in *maildir-info*
		collect (stumpwm:format-expand `((#\l ,(constantly label))
										 (#\n ,(lambda () (format nil "^[~@[^B~*~]~D^]"
																  (plusp (getf info :new))
																  (getf info :new))))
										 (#\c ,(lambda () (format nil "~D" (getf info :cur))))
										 (#\t ,(lambda () (format nil "~D" (getf info :tmp)))))
									   *maildir-modeline-fmt*)
		  into fmts
		finally (return (apply #'concat fmts))))



(stumpwm:add-screen-mode-line-formatter #\D #'maildir-modeline)
