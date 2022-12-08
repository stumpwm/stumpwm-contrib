;;;; swm-ssh.lisp

(defpackage #:swm-ssh
  (:use #:cl #:stumpwm)
  (:export #:*swm-ssh-default-term*
           #:*swm-ssh-config-path*)
  (:import-from #:cl-ppcre))

(in-package #:swm-ssh)

(defvar *swm-ssh-config-path* #p"~/.ssh/config")

(defvar *host-regex* "^(?i)Host[ \t]+")

(defvar *swm-ssh-default-term* "urxvtc")

(defun collect-hosts (&optional (ssh-config *swm-ssh-config-path*))
  (with-open-file (stream ssh-config :direction :input)
    (loop for line = (read-line stream nil)
          while line
          when (cl-ppcre:scan *host-regex* line)
            collect (cl-ppcre:regex-replace-all *host-regex* line ""))))

(stumpwm:defcommand swm-ssh-menu () ()
  "Select a host to ssh to"
  (let ((entry (stumpwm:select-from-menu
                (stumpwm:current-screen)
                (mapcar 'list (collect-hosts))
                "Open ssh connection to: ")))
    (when entry
      (stumpwm:run-shell-command (format nil "~A -e ssh ~A" *swm-ssh-default-term* (car entry))))))
