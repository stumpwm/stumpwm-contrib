(in-package :swm-clim-message)

(defun windowlist (&key persist)
  "Similar to the StumpWM WINDOWLIST command, this lists the windows of the
current group, formatted according to STUMPWM:*WINDOW-FORMAT*."
  (let* ((fmt stumpwm:*window-format*)
         (group (stumpwm:current-group))
         (windows (stumpwm::sort-windows-by-number
                   (stumpwm:group-windows group))))
    (clim-message
     (generate-clim-message
      windows
      (lambda (w) (stumpwm:format-expand stumpwm:*window-formatters* fmt w))
      (lambda (w) (stumpwm:group-focus-window group w) w))
     :repeatable-actions persist
     :highlight 0
     :bind-keys t)))

(defun select-window-from-menu (windows &optional (fmt stumpwm:*window-format*))
  (select-from-menu windows
                    :formatter (lambda (w)
                                 (stumpwm:format-expand stumpwm:*window-formatters*
                                                        fmt
                                                        w))
                    :highlight 0))
