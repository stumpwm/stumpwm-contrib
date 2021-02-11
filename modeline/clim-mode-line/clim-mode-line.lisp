;;;; clim-mode-line.lisp

(in-package #:clim-mode-line)

(defvar *mode-line-active* nil)
(defvar *default-mode-line-function* 'format-mode-line)
(defparameter *mode-line-active-formatters* '((format-groups format-windows)))

(define-application-frame clim-mode-line () ()
  (:panes (display :application
		   :display-function 'clim-mode-line-display-function
		   ;; :width 1920
		   ;; :height 10
                   ;; :incremental-redisplay t
		   :scroll-bars nil
		   :borders nil)
	  ;; We should add an execute-extended-command, and switch to a layout containing that when
	  ;; one calls colon. 
	  ;; (colon :modeline)
          )
  (:layouts
   (default display)
   ;; (input colon)
   ;; (display+input (horizontally ()
   ;;      	    display colon))
   ))

(define-clim-mode-line-command (com-quit) ()
  (frame-exit (or *application-frame*
		  (find-application-frame 'clim-mode-line :create nil :activate nil))))

(defun clim-mode-line-display-function (frame pane)
  "This just calls the function stored in *default-mode-line-function*"
  (funcall *default-mode-line-function* frame pane))

(defun mode-line-active? ()
  *mode-line-active*)

(defun run-mode-line (&key (height 20))
  (let ((frame (make-application-frame 'clim-mode-line
				       :height height)))
    (setf *mode-line-active* t)
    (run-frame-top-level frame)))

(defvar *redisplay-mode-line-lock* (bt:make-lock "cml-lock"))

(defun redisplay-clim-mode-line (&optional (ml (find-application-frame
                                                'clim-mode-line
                                                :create nil
                                                :activate nil)))
  (bt:with-lock-held (*redisplay-mode-line-lock*)
    (when ml
      (redisplay-frame-panes ml :force-p t))))

(defmethod clime:find-frame-type ((frame clim-mode-line)) 
  "this method sets the window type via clim-clx::adopt-frame as defined below."
  :dock)
