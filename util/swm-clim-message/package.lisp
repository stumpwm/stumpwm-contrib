;;;; package.lisp

(defpackage #:swm-clim-message
  (:use #:clim #:clim-lisp)
  (:export #:clim-message
           #:generate-clim-message
           #:message
           #:select-from-menu
           #:message-window-current-value
           #:message-window-final-value
           #:*clim-message-window-keymap*
           #:swm-clim-message-command
           #:windowlist
           #:select-window-from-menu))
