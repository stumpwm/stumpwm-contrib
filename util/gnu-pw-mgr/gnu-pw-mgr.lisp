;;;; gnu-pw-mgr.lisp
;; Copyright (C) 2019  Brandon Invergo <brandon@invergo.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:gnu-pw-mgr)

;; All timer code is blatantly stolen^H^H^H^H^H^Hborrowed from the
;; passwd module.
(defvar *password-id-remember-timeout* 0
  "How long will the password-id be remembered (in minutes)")
(defvar *clipboard-clear-timeout* 10
  "How long will the password-id be remembered (in seconds)")

(defvar *password-id* nil)

(defvar *password-id-timer*
  (sb-ext:make-timer (lambda ()
                       (setf *password-id* nil))))

(defvar *old-clipboard* nil)

(defvar *clipboard-timer*
  (sb-ext:make-timer (lambda ()
                       (set-x-selection *old-clipboard*)
                       (setf *old-clipboard* nil))))

(defun reset-timer (timer timeout)
  (when (sb-ext:timer-scheduled-p timer)
    (sb-ext:unschedule-timer timer))
  (sb-ext:schedule-timer timer timeout))

(stumpwm:define-stumpwm-type :gpw-password-id (input prompt)
  (or *password-id*
      (setf *password-id*
            (or (argument-pop-rest input)
                (read-one-line (current-screen) prompt :password t)))))

(defun parse-id-lines (lines)
  (loop :for index :from 1
        :for id-line :in lines
        :for id := (first (cl-ppcre:split " +" id-line))
        :collect (format nil "~D) ~A" index id)))

(defun label-line-p (line)
  (cl-ppcre:scan "^$|^seed-tag|^login id hint:" line))

(stumpwm:defcommand password-to-selection (pwid)
  ((:gpw-password-id "Password ID: "))
  "Prompt for a password ID and a seed ID and set the X selection to
the resulting password."
  (let* ((cmd (format nil "exec gnu-pw-mgr '~A'" pwid))
         (output (stumpwm:run-shell-command cmd t))
         (lines (remove-if 'label-line-p (cl-ppcre:split "\\n" output)))
         (seed (select-from-menu (stumpwm:current-screen)
                                 (parse-id-lines lines)
                                 "seed ID:"))
         (seedno (when seed
                     (parse-integer (first (cl-ppcre:split "\\) " seed))))))
    (unless *old-clipboard*
      (setf *old-clipboard* (get-x-selection)))
    (when (and *clipboard-clear-timeout*
               (> *clipboard-clear-timeout* 0))
      (reset-timer *clipboard-timer* *clipboard-clear-timeout*))
    (if (and *password-id-remember-timeout*
             (> *password-id-remember-timeout* 0))
        (reset-timer *password-id-timer* (* *password-id-remember-timeout* 60))
        (setf *password-id* nil))
    (when seedno
      (stumpwm:set-x-selection
       (second (cl-ppcre:split " +" (nth (1- seedno) lines))))
      (alexandria:when-let ((string (cl-ppcre:scan-to-strings "login id hint: .+\\n" output)))
        (stumpwm:message string)))))
