
(uiop:define-package #:swm/gmk
  (:use :cl)
  (:export set-modifier-actions
           *modifiers*
           modifier-key-release-handler
           modifier-key-press-handler))

(in-package #:swm/gmk)

(stumpwm:define-minor-mode grabbable-modifier-keys-mode () ()
  (:documentation
   "When enabled, modifiers specified by the user are grabbed and functions fired
on their press and release.")
  (:scope :unscoped)
  (:interactive make-modifiers-grabbable)
  (:lighter "GMK")
  (:expose-keymaps t))

(defvar *modifiers-to-grab* nil
  "Set by the user to control which mods are grabbable")

 ; grabbable-modifier-keys actions, called on modifier key press and release
   ; with both the code and the state
(defparameter *gmk-on-mod-press-release* nil
  "Internal variable, hold a plist with modifiers as keys and a cons of
press/release functions as the value.")

(defun set-modifier-actions (mod press release)
  "Add or replace a modifiers press and release actions. PRESS and RELEASE must be
functions of arity 2 which take the key code and state mask."
  (pushnew mod *modifiers-to-grab*)
  (setf (getf *gmk-on-mod-press-release* mod) (cons press release))
  (when (and (null press) (null release))
    (setf *modifiers-to-grab* (remove mod *modifiers-to-grab*))))

(defun invoke-on-press-action (mod code state)
  (let ((todo (getf *gmk-on-mod-press-release* mod)))
    (when (car todo)
      (values (funcall (car todo) code state) t))))

(defun invoke-on-release-action (mod code state)
  (let ((todo (getf *gmk-on-mod-press-release* mod)))
    (when (cdr todo)
      (values (funcall (cdr todo) code state) t))))

 ; Our modifier map
(defvar *modifiers* nil
  "Map our modifiers to their individual keys so we can grab them")

(defun fill-*modifiers* ()
  "Fill in our modifier map"
  (multiple-value-bind (shift lock control mod1 mod2 mod3 mod4 mod5)
      (xlib:modifier-mapping stumpwm:*display*)
    (setf *modifiers*
          (list :shift shift
                :lock lock
                :control control
                :mod1 mod1
                :mod2 mod2
                :mod3 mod3
                :mod4 mod4
                :mod5 mod5))))

(defun grab-modifier-keys (modifier &optional ungrab)
  "Grab or ungrab modifier keys for the screen root"
  (if ungrab
      (loop for key-code in (getf *modifiers* modifier)
            do (xlib:ungrab-key (stumpwm:screen-root (stumpwm:current-screen))
                                key-code))
      (loop for key-code in (getf *modifiers* modifier)
            do (xlib:grab-key (stumpwm:screen-root (stumpwm:current-screen))
                              key-code))))

(defun compute-which-mod (code)
  "Given CODE, find which modifier it belongs to"
  (let ((which-mod
          (loop for (mod codes) on *modifiers* by #'cddr
                when (member code codes :test #'=)
                  collect mod)))
    (when (cdr which-mod)
      (warn "Multiple mods defined for key code ~D" code))
    (car which-mod)))

(defun modifier-key-release-handler (code state)
  (let ((which-mod (compute-which-mod code)))
    (multiple-value-bind (res found)
        (invoke-on-release-action which-mod code state)
      (when found
        (values t res)))))

(defun modifier-key-press-handler (code state)
  (let ((which-mod (compute-which-mod code)))
    (multiple-value-bind (res found)
        (invoke-on-press-action which-mod code state)
      (when found
        (values t res)))))

(defun on-enter (sym obj)
  (declare (ignore sym obj))
  (fill-*modifiers*)
  (setf stumpwm::*custom-key-event-handler* 'modifier-key-press-handler)
  (dolist (mod *modifiers-to-grab*)
    (grab-modifier-keys mod)))

(defun on-exit (sym obj)
  (declare (ignore sym obj))
  (setf stumpwm::*custom-key-event-handler* nil)
  (dolist (mod *modifiers-to-grab*)
    (grab-modifier-keys mod t)))

(stumpwm:add-hook *grabbable-modifier-keys-mode-enable-hook* 'on-enter)
(stumpwm:add-hook *grabbable-modifier-keys-mode-disable-hook* 'on-exit)

(in-package :stumpwm)

(define-stump-event-handler :key-release (code state)
  (declare (ignorable code state))
  (dformat 1 "~&>>> Key Release ~A ~A~&" code state)
  (swm/gmk::modifier-key-release-handler code state))
