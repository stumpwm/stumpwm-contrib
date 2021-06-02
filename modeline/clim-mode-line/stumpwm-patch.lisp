(in-package :stumpwm)

;;; We want to redisplay our clim mode line every time a command is run.

(defun refresh-clim-mode-line-hook-fn (&rest ignore)
  (declare (ignore ignore))
  (handler-case 
      (clim-mode-line::redisplay-clim-mode-line)
    (error () nil)))

(add-hook *event-processing-hook* 'refresh-clim-mode-line-hook-fn)

