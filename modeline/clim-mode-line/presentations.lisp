(in-package :clim-mode-line)

(defvar *current-group-color* +red+)
(defvar *current-window-color* +red+)

;;; Presentation Methods

(define-presentation-method present
    (window (type stumpwm::window) stream view &key)
  (let ((str (string-trim '(#\space) (stumpwm::format-expand
                                      stumpwm::*window-formatters*
                                      stumpwm::*window-format*
                                      window)))
        (curwin (stumpwm:current-window)))
    (if (eq curwin window)
        (with-drawing-options (stream :ink *current-window-color*)
          (format stream "~A" str))
        (format stream "~A" str))))

(define-presentation-method present
    (group (type stumpwm::group) stream view &key)
  (let ((curgrp (stumpwm:current-group)))
    (if (eq curgrp group)
        (with-drawing-options (stream :ink *current-group-color*)
          (format stream "~A" (stumpwm:group-name group)))
        (format stream "~A" (stumpwm:group-name group)))))

;;; Commands

(define-clim-mode-line-command (com-move-to-group)
    ((group stumpwm::group))
  (stumpwm::switch-to-group group))

(define-clim-mode-line-command (com-kill-group)
    ((dead-group stumpwm::group))
  (let* ((groups (stumpwm:screen-groups (stumpwm:current-screen)))
         (to-group (or (stumpwm::next-group dead-group
                                            (stumpwm::non-hidden-groups groups))
                       (stumpwm::next-group dead-group groups))))
    (if to-group
        (let ((dead-group-name (stumpwm:group-name dead-group)))
          (stumpwm::switch-to-group to-group)
          (stumpwm::kill-group dead-group to-group)
          (stumpwm::message "Deleted ~a." dead-group-name))
        (stumpwm::message "There's only one group left."))))

(define-clim-mode-line-command (com-kill-group-dragged)
    ((dead-group stumpwm::group) (to-group stumpwm::group))
  (stumpwm::switch-to-group to-group)
  (stumpwm::kill-group dead-group to-group)
  (stumpwm::message "Deleted ~a." dead-group-name))

(define-clim-mode-line-command (com-move-window-into-group)
    ((window stumpwm::window) (group stumpwm::group))
  (stumpwm::message "moving window ~a into group ~a"
                    (stumpwm::window-name window)
                    (stumpwm::group-name group))
  (handler-case (stumpwm::pull-to-group window group)
    (error ()
      (stumpwm::message "an error was encountered"))))

(define-clim-mode-line-command (com-focus-window)
    ((window stumpwm::window))
  (stumpwm::group-focus-window (stumpwm::window-group window) window))

(define-clim-mode-line-command (com-delete-window)
    ((window stumpwm::window))
  (stumpwm:delete-window window))

(define-clim-mode-line-command (com-kill-window)
    ((window stumpwm::window))
  (stumpwm:kill-window window))

(define-clim-mode-line-command (com-renumber-window)
    ((window stumpwm::window))
  (let ((group (window-group window))
        (nf (window-number window))
        (win (find-if #'(lambda (win)
                          (= (window-number win) nt))
                      (group-windows group))))
    (if win
        (progn
          (setf (window-number win) nf)
          (setf (window-number window) nt))
        (setf (window-number window) nt))))

;;; Gestures

(define-gesture-name :meta-left-click :pointer-button-press (:left :meta))

;;; presentation to command translators

(define-presentation-to-command-translator cml-select-group
    (stumpwm::group com-move-to-group clim-mode-line
     :priority 9)
    (group)
  (list group))

(define-presentation-to-command-translator cml-kill-group
    (stumpwm::group com-kill-group clim-mode-line
     :priority 0)
    (group)
  (list group))

(define-presentation-to-command-translator cml-select-window
    (stumpwm::window com-focus-window clim-mode-line
     :priority 9)
    (window)
  (list window))

(define-presentation-to-command-translator cml-delete-window
    (stumpwm::window com-delete-window clim-mode-line
     :priority 1 :gesture :meta-left-click)
    (window)
  (list window))

;;; D&D translators

(define-drag-and-drop-translator cml-move-window-to-group
    (stumpwm::window command stumpwm::group clim-mode-line)
    (object destination-object)
  `(com-move-window-into-group ,object ,destination-object))

(define-drag-and-drop-translator cml-group-absorb-window
    (stumpwm::group command stumpwm::window clim-mode-line)
    (object destination-object)
  `(com-move-window-into-group ,destination-object ,object))
