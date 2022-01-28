(in-package #:clim-mode-line)

(define-mode-line-command (com-refresh) () nil)

(define-mode-line-command (com-quit) ()
  (frame-exit *application-frame*))

;;; Define commands followed by their translators.

;; Click on a group to switch to it
(define-mode-line-command (com-switch-to-group)
    ((group stumpwm::group))
  (stumpwm::switch-to-group group))

(define-presentation-to-command-translator mode-line-switch-to-group
    (stumpwm::group com-switch-to-group mode-line
     :gesture :left-click
     :priority 9
     :documentation "Switch to the group")
    (group)
  (list group))

;; Click on a window to focus it.
(define-mode-line-command (com-focus-all)
    ((window stumpwm::window))
  (stumpwm::focus-all window))

(define-presentation-to-command-translator mode-line-focus-window
    (stumpwm::window com-focus-all mode-line
     :gesture :left-click
     :priority 9)
    (window)
  (list window))

;; Drag a window to a group to move it there
(define-mode-line-command (com-move-window-to-group)
    ((window stumpwm::window) (group stumpwm::group))
  (stumpwm::move-window-to-group window group))

(define-drag-and-drop-translator mode-line-move-window-to-group
    (stumpwm::window command stumpwm::group mode-line
                     :gesture :left-click-control
                     :menu nil)
    (window destination-object)
  `(com-move-window-to-group ,window ,destination-object))

;; Swap two windows in their frames
(define-mode-line-command (com-swap-windows)
    ((w1 stumpwm::window) (w2 stumpwm::window))
  (let ((focused (stumpwm:current-window))
        (f1 (stumpwm::window-frame w1))
        (f2 (stumpwm::window-frame w2)))
    (stumpwm::pull-window w1 f2)
    (stumpwm::pull-window w2 f1)
    (stumpwm:focus-window focused t)))

(define-drag-and-drop-translator mode-line-swap-windows
    (stumpwm::window command stumpwm::window mode-line
                     :gesture :left-click-control
                     :menu nil)
    (window destination-object)
  `(com-swap-windows ,window ,destination-object))

;; Swap window numbers
(define-mode-line-command (com-swap-window-numbers)
    ((w1 stumpwm::window) (w2 stumpwm::window))
  (let ((n1 (stumpwm:window-number w1))
        (n2 (stumpwm:window-number w2)))
    (setf (stumpwm:window-number w1) n2
          (stumpwm:window-number w2) n1)))

(define-drag-and-drop-translator mode-line-swap-window-numbers
    (stumpwm::window command stumpwm::window mode-line
                     :gesture :right-click-meta
                     :menu nil)
    (window destination-object)
  `(com-swap-window-numbers ,window ,destination-object))

;; Swap group numbers
(define-mode-line-command (com-swap-group-numbers)
    ((g1 stumpwm::group) (g2 stumpwm::group))
  (let ((n1 (stumpwm:group-number g1))
        (n2 (stumpwm:group-number g2)))
    (setf (stumpwm:group-number g1) n2
          (stumpwm:group-number g2) n1)))

(define-drag-and-drop-translator mode-line-swap-group-numbers
    (stumpwm::group command stumpwm::group mode-line
                    :gesture :left-click-control
                    :menu nil)
    (group destination-object)
  `(com-swap-group-numbers ,group ,destination-object))

;; Merge two groups together

(define-mode-line-command (com-merge-groups)
    ((g1 stumpwm::group) (g2 stumpwm::group))
  (stumpwm::merge-groups g1 g2))

(define-drag-and-drop-translator mode-line-merge-groups
    (stumpwm::group command stumpwm::group mode-line
                    :gesture :right-click-control
                    :menu nil)
    (group destination-object)
  `(com-merge-groups ,group ,destination-object))

;; Kill a group and merge it into another
(define-mode-line-command (com-delete-group-merge-into)
    ((g1 stumpwm::group) (g2 stumpwm::group))
  (stumpwm::kill-group g1 g2))

(define-drag-and-drop-translator mode-line-delete-group-merge-groups
    (stumpwm::group command stumpwm::group mode-line
                    :gesture :right-click-meta
                    :menu nil)
    (group destination-object)
  `(com-delete-group-merge-into ,group ,destination-object))


