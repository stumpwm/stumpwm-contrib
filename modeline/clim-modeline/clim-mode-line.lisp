;;;; clim-mode-line.lisp

(in-package #:clim-mode-line)

(defvar *mode-line-active* nil)
(defvar *default-mode-line-function* 'format-mode-line)
(defparameter *mode-line-active-formatters* '((format-nil format-groups format-windows)))

;; (defun do-stumpwm-cmd (string)
;;   )

(define-application-frame clim-mode-line () ()
  ;; (:top-level )
  (:panes (display :application
		   :display-function 'clim-mode-line-display-function
		   ;; :width 1920
		   ;; :height 10
		   :scroll-bars nil
		   :borders nil))
  (:layouts (default display)))

(defun clim-mode-line-display-function (frame pane)
  "This just calls the function stored in *default-mode-line-function*"
  (funcall *default-mode-line-function* frame pane))

(defun mode-line-active? ()
  *mode-line-active*)

(defun run-mode-line ()
  (let ((frame (make-application-frame 'clim-mode-line
				       :height 20)))
    (setf *mode-line-active* t)
    (run-frame-top-level frame)))

(defun redisplay-clim-mode-line (&optional (ml (find-application-frame 'clim-mode-line :create nil :activate nil)))
  (when ml
    (redisplay-frame-panes ml :force-p t)))

(defmethod clime:find-frame-type ((frame clim-mode-line)) 
  "this method sets the window type via clim-clx::adopt-frame as defined below."
  :dock)

(defmethod default-frame-top-level
    ((frame clim-mode-line)
     &key (command-parser 'command-line-command-parser)
       (command-unparser 'command-line-command-unparser)
       (partial-command-parser
	'command-line-read-remaining-arguments-for-partial-command)
       (prompt "Command: "))
  (declare (ignore prompt))
  ;; Give each pane a fresh start first time through.
  (loop
      ;; The variables are rebound each time through the loop because the
      ;; values of frame-standard-input et al. might be changed by a command.
      ;;
      ;; We rebind *QUERY-IO* ensuring variable is always a stream,
      ;; but we use FRAME-QUERY-IO for our own actions and to decide
      ;; whenever frame has the query IO stream associated with it..
      (let* ((frame-query-io (frame-query-io frame))
	     (*standard-input*  (or (frame-standard-input frame)  *standard-input*))
	     (*standard-output* (or (frame-standard-output frame) *standard-output*))
	     (*query-io* (or frame-query-io *query-io*))
	     ;; during development, don't alter *error-output*
	     ;; (*error-output* (frame-error-output frame))
	     (*pointer-documentation-output* (frame-pointer-documentation-output frame))
	     (*command-parser* command-parser)
	     (*command-unparser* command-unparser)
	     (*partial-command-parser* partial-command-parser))
	(restart-case
	    (flet ((execute-command ()
		     (let ((command (read-frame-command frame :stream frame-query-io)))
		       (when command
			 (execute-frame-command frame command)))))
	      (redisplay-frame-panes frame :force-p t)
	      (execute-command))
	  (abort ()
	    :report "Return to application command loop."
	    (beep))))))
