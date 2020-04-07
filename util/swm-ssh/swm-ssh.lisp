;;;; swm-ssh.lisp

(defpackage #:swm-ssh
  (:use #:cl #:stumpwm)
  (:export :*swm-ssh-default-term*
           :*swm-ssh-config-path*)
  (:import-from #:cl-ppcre))

(in-package #:swm-ssh)

(defvar *swm-ssh-config-path* #p"~/.ssh/config")

(defvar *swm-ssh-host-regex* "^Host")

(defvar *swm-ssh-default-term* "urxvtc")

(defun swm-ssh-read-config ()
  (with-open-file (stream *swm-ssh-config-path* :direction :input)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun swm-ssh-collect-hosts ()
  "Return a list of available hosts after stripping *swm-ssh-host-regex*"
  (let ((ptrn (cl-ppcre:create-scanner *swm-ssh-host-regex*))
        (host-list '(nil)))
    (loop for line in (swm-ssh-read-config)
          while line
          when (cl-ppcre:scan *swm-ssh-host-regex* line)
            collect (cl-ppcre:regex-replace-all *swm-ssh-host-regex* line ""))))

(stumpwm:defcommand swm-ssh-menu () ()
  "Select a host to ssh to"
  (let ((entry (stumpwm:select-from-menu
                (stumpwm:current-screen)
                (mapcar 'list (swm-ssh-collect-hosts))
                "Open ssh connection to: ")))
    (when entry
      (stumpwm:run-shell-command (format nil (concatenate 'string *swm-ssh-default-term* " -e ssh " (car entry)))))))
