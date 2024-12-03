;;;; swm-ssh.lisp

(defpackage #:swm-ssh
  (:use #:cl #:stumpwm)
  (:export #:*swm-ssh-default-term*
           #:*swm-ssh-known-hosts-path*)
  (:import-from #:cl-ppcre))

(in-package #:swm-ssh)

(defvar *swm-ssh-known-hosts-path* #p"~/.ssh/known_hosts")

(defvar *host-regex* "^([^ :]+)( |\\t).+")

(defvar *swm-ssh-default-term* "urxvtc")

(defun collect-hosts (&optional (ssh-known-hosts *swm-ssh-known-hosts-path*))
  (with-open-file (stream ssh-known-hosts :direction :input)
    (loop for line = (read-line stream nil)
          while line
          when (cl-ppcre:scan *host-regex* line)
            collect (cl-ppcre:regex-replace-all *host-regex* line "\\1"))))

(stumpwm:defcommand swm-ssh-menu () ()
  "Select a host to ssh to"
  (let ((entry (stumpwm:select-from-menu
                (stumpwm:current-screen)
                (mapcar 'list
                        (delete-duplicates (collect-hosts)
                                           :test #'equal))
                "Open ssh connection to: ")))
    (when entry
      (let ((host (car entry)))
        (stumpwm:run-shell-command
         (format nil "~A -T ~A -e ssh ~A" *swm-ssh-default-term* host host))))))
