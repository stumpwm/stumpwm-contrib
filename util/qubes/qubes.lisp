;;;; qubes.lisp

(in-package #:qubes)

;; Qubes module for StumpWM.
;;
;; Copyright (C) 2016 Johanna Abrahamsson
;;
;; Maintainer: Johanna Abrahamsson
;;

;;; Code:

(defun alloc-color-on-current (color)
  (stumpwm::alloc-color (current-screen) color))


(defparameter *label-colors*
  (list (alloc-color-on-current "pink1")
        (alloc-color-on-current "red1")
        (alloc-color-on-current "orange1")
        (alloc-color-on-current "yellow1")
        (alloc-color-on-current "green1")
        (alloc-color-on-current "gray1")
        (alloc-color-on-current "blue1")
        (alloc-color-on-current "purple1")
        (alloc-color-on-current "black")))


(defun window-label-color (window)
  (nth (or (first (xlib:get-property (window-xwin window) :_QUBES_LABEL))
           0)
       *label-colors*))


(defun window-vm (window)
  (let ((wmproperty (xlib:get-property (window-xwin window) :_QUBES_VMNAME)))
    (unless (= (length wmproperty) 0)
      (stumpwm::utf8-to-string wmproperty))))

(defun window-wmname (window)
  (let ((wmproperty (xlib:get-property window :WM_NAME)))
    (unless (= (length wmproperty) 0)
      (stumpwm::utf8-to-string wmproperty))))


(defmethod stumpwm::update-decoration ((stumpwm::window stumpwm::tile-window))
  ;; give it a colored border but only if there are more than 1 frames.
  (let* ((group (window-group window))
         (screen (group-screen group)))
    (let ((c (or (window-label-color window) (screen-unfocus-color screen))))
      (setf (xlib:window-border (window-parent window)) c
            ;; windows that dont fill the entire screen have a transparent background.
            (xlib:window-background (window-parent window))
            (if (eq (window-type window) :normal)
                (if (eq *window-border-style* :thick)
                    c
                    (screen-win-bg-color screen))
                :none))
      ;; get the background updated
      (xlib:clear-area (window-parent window)))))

(defun stumpwm::xwin-name (win)
  (stumpwm::escape-caret (or (stumpwm::xwin-net-wm-name win)
                             (xlib:wm-name win)
                             (window-wmname win))))


(defun stumpwm::window-name (window)
  (concatenate 'string
               "["
               (or (window-vm window) "Dom0")
               "] "
               (or (stumpwm:window-user-title window)
                   (case *window-name-source*
                     (:resource-name (window-res window))
                     (:class (window-class window))
                     (t (window-title window)))
                   *default-window-name*)))

;;; End of file
