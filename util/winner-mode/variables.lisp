(in-package #:winner-mode)

(defvar *current-ids* (make-hash-table))
(defvar *max-ids* (make-hash-table))
(defvar *tmp-folder* #p"/tmp/")
(defvar *default-commands*
  '(stumpwm:only
    stumpwm:pull-from-windowlist
    stumpwm:pull-hidden-next
    stumpwm:pull-hidden-other
    stumpwm:pull-hidden-previous
    stumpwm:pull-marked
    stumpwm:pull-window-by-number
    stumpwm:next
    stumpwm:next-in-frame
    stumpwm:next-urgent
    stumpwm:prev
    stumpwm:prev-in-frame
    stumpwm:select-window
    stumpwm:select-from-menu
    stumpwm:select-window-by-name
    stumpwm:select-window-by-number
    stumpwm::pull
    stumpwm::remove
    stumpwm:iresize
    stumpwm:vsplit
    stumpwm:hsplit
    stumpwm:move-window
    stumpwm:move-windows-to-group
    stumpwm:move-window-to-group
    stumpwm:balance-frames
    stumpwm::delete
    stumpwm::kill
    stumpwm:fullscreen))
