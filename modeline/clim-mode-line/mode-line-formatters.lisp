(in-package :clim-mode-line)

(defun format-groups (frame pane)
  (declare (ignore frame))
  (slim:cell (format pane "["))
  (loop for group in (stumpwm::sort-groups (stumpwm:current-screen))
        with current = (stumpwm:current-group)
        do (slim:cell
             (if (eql group current)
                 (with-drawing-options (pane :ink +red+)
                   (present group 'stumpwm::group :stream pane :single-box t))
                 (present group 'stumpwm::group :stream pane :single-box t))))
  (slim:cell (format pane "]")))

(defun format-windows (frame pane)
  (declare (ignore frame))
  (slim:cell (format pane "["))
  (loop for window in (stumpwm::sort-windows (stumpwm:current-group))
        with current = (stumpwm:current-window)
        do (slim:cell (if (eql window current)
                          (with-drawing-options (pane :ink +red+)
                            (present window 'stumpwm::window :stream pane
                                                             :single-box t))
                          (present window 'stumpwm::window :stream pane
                                                           :single-box t))))
  (slim:cell (format pane "]")))

(defun format-mode-line (frame pane)
  "As the default mode line formatting function, format-mode-line walks through the list of active 
formatters and calls them within the appropriate row/column configuration. "
  (let ((flists (if (functionp (car *mode-line-active-formatters*))
		    (list *mode-line-active-formatters*)
		    *mode-line-active-formatters*)))
    ;; (dragging-output (pane)) ; useful? 
    (slim:with-table (pane)
      (loop for flist in flists
	    do (slim:row
		 (loop for fn in flist
		       do (slim:col (funcall fn frame pane))))))))

