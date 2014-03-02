;;;; productivity.lisp

(in-package #:productivity)

;;; "productivity" goes here. Hacks and glory await!

;; Productivity module for StumpWM.
;;
;; Copyright (C) 2008 Ivy Foster
;;
;; Maintainer: Ivy Foster
;;

;;; Code:

(defvar *productivity-mode-is-on* nil
  "Is productivity-mode on?
Do not customize by hand unless you're crazy.")

(defvar *productivity-keys* '(("C-t" *root-map*))
  "List of all the keys you have bound to your `*top-map*'
and their associated commands.")

(defvar *productivity-stop-message* "Break time!"
  "What should StumpWM print when you stop productivity-mode?")

(defvar *productivity-start-message* "Get to work!"
  "What should StumpWM print when you start productivity-mode?")

(defvar *productivity-back-to-work-message* "Get back to work!"
  "What should StumpWM print when you attempt to waste time?")

(defcommand productivity-back-to-work () ()
  (message *productivity-back-to-work-message*))

(defun productivity-mode-on ()
  "Turns on productivity mode. Do not call interactively."
  (setf *productivity-mode-is-on* t)
  (dolist (key *productivity-keys*)
    (define-key *top-map* (kbd (car key)) "productivity-back-to-work"))
  (message *productivity-start-message*))

(defun productivity-mode-off ()
  "Turns off productivity mode. Do not call interactively."
  (setf *productivity-mode-is-on* nil)
  (dolist (key *productivity-keys*)
    (define-key *top-map* (kbd (car key)) (cadr key)))
  (message *productivity-stop-message*))

(defcommand productivity-mode-toggle () ()
  "Toggles productivity mode."
  (if *productivity-mode-is-on*
      (productivity-mode-off)
    (productivity-mode-on)))

;;; End of file
