;;;; swm-ssh.lisp

(defpackage #:swm-ssh
  (:use #:cl #:stumpwm)
  (:export #:*swm-ssh-default-term*
           #:*swm-ssh-config-path*)
  (:import-from #:cl-ppcre))

(in-package #:swm-ssh)

(defvar *swm-ssh-config-path* #p"~/.ssh/config")

(defvar *swm-ssh-host-regex* "^Host[ \t]+")

(defvar *swm-ssh-default-term* "urxvtc")

(defun collect-hosts (&optional (ssh-config *swm-ssh-config-path*))
  (with-open-file (stream ssh-config :direction :input)
    (loop for line = (read-line stream nil)
          while line
          when (cl-ppcre:scan *swm-ssh-host-regex* line)
            collect (cl-ppcre:regex-replace-all *swm-ssh-host-regex* line ""))))

(stumpwm:defcommand swm-ssh-menu () ()
  "Select a host to ssh to"
  (alexandria:when-let ((entry (stumpwm:select-from-menu (stumpwm:current-screen)
                                                         (mapcar 'list (swm-ssh-collect-hosts))
                                "Open ssh connection to: ")))
    (stumpwm:run-shell-command (format nil "~A -e ssh ~A" *swm-ssh-default-term* (car entry))))
