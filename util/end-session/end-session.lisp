;;;; session-ending.lisp

(in-package #:end-session)

;;; Commands to exit out of stumpwm whie closing open programs nicely
;;;

;;; Copyright (c) 2018 Stuart Dilts
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;


(defun yes-no-diag (query-string)
  "Presents a yes-no dialog to the user asking query-string.
Returns true when yes is selected"
  (equal :yes (cadr (select-from-menu (current-screen)
                            '(("No" :no) ("Yes" :yes))
                            query-string))))


(defcommand suspend-computer () ()
  "Suspends the computer"
  (let ((choice (yes-no-diag "Really suspend?")))
    (when choice
      (echo-string (current-screen) "Suspending...")
      (run-shell-command "loginctl suspend"))))

(defun close-all-apps ()
  "Closes all windows managed by stumpwm gracefully"
  ;; yes, this uses an external tool instead of stumpwm internals
  (let ((win-index-text (run-shell-command "wmctrl -l | awk '{print $1}'" t)))
    (dolist (window (cl-ppcre:split "\\\n" win-index-text))
      (run-shell-command (format nil "wmctrl -i -c ~A" window)))))

(defcommand shutdown-computer () ()
  (let ((choice (yes-no-diag "Really Shutdown? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Shutting down...")
      (close-all-apps)
      (run-hook *quit-hook*)
      (run-shell-command "loginctl poweroff"))))

;; can't name the function "restart"
(defcommand restart-computer () ()
  (let ((choice (yes-no-diag "Really Restart? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Restarting...")
      (close-all-apps)
      (run-hook *quit-hook*)
      (run-shell-command "loginctl reboot"))))

(defcommand logout () ()
  (let ((choice (yes-no-diag "Close all programs and quit stumpwm?")))
    (when choice
      (echo-string (current-screen) "Ending Session...")
      (close-all-apps)
      (run-hook *quit-hook*)
      (quit))))

(defcommand end-session () ()
  (let ((choice (select-from-menu (current-screen) *end-session-menu*
                               "Quit Session?")))
    (when choice
      (apply (second choice) nil))))

(defvar *end-session-menu*
  (list (list "Logout" #'logout)
        (list "Shutdown" #'shutdown-computer)
        (list "Restart"  #'restart-computer)
        (list "Suspend" #'suspend-computer))
  "The options that are available to quit a stumpwm session.
Entries in the list has the format of (\"item in menu\" #'function-to-call)")
