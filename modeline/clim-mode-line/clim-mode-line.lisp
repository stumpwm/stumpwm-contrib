(in-package #:clim-mode-line)

;; Uncomment for debugging
;; (declaim (optimize (speed 0)
;;                    (safety 3)
;;                    (debug 3)))

(defvar *align-x* :left)

(defvar *highlight-drop* nil)

(defvar *display-as-table* nil)

(defvar *display-style* :text
  "Should be one of either :table or :text.")

(defvar *text-display-formatter-intermix* " "
  "Spacing between each formatters output when in text mode. This string is 
formatted to the output stream in between each formatter when in text mode.")

(defvar *stumpwm-modeline-frame* nil
  "Hold the single mode line")

(define-application-frame mode-line ()
  ((formatters :initform (list (list 'format-groups
                                     'format-align-right
                                     'format-windows))
               :initarg :formatters
               :accessor mode-line-formatters)
   (highlight-color :initform +red+
                    :initarg :highlight-color
                    :accessor mode-line-highlight-color)
   (foreground-color :initform +black+
                     :initarg :foreground-color
                     :accessor mode-line-foreground-color)
   (mutex :initform (sb-thread:make-mutex :name "clim-modeline-mutex")
          :accessor mode-line-mutex)
   (head-width :initarg :head-width
               :accessor mode-line-head-width))
  (:panes (display :application
                   :display-function 'display-mode-line
                   :scroll-bars nil
                   :borders nil))
  (:layouts
   (default display)))

(defmethod (setf mode-line-formatters) (new (frame mode-line))
  (sb-thread:with-mutex ((mode-line-mutex frame))
    (setf (slot-value frame 'formatters) new)
    (redisplay-frame-panes frame :force-p t)
    (stumpwm:call-in-main-thread
     (lambda ()
       (sb-thread:with-mutex ((mode-line-mutex frame))
         (let* ((sheet (frame-top-level-sheet frame))
                (space (compose-space sheet))
                (width (space-requirement-width space))
                (height (space-requirement-height space)))
           (move-and-resize-sheet sheet 0 0 width height))
         (mapcar 'stumpwm::resize-mode-line
                 stumpwm::*mode-lines*)
         (mapcar (lambda (group)
                   (mapcar (lambda (head)
                             (stumpwm::group-sync-head
                              group head))
                           (stumpwm::group-heads group)))
                 (stumpwm::screen-groups
                  (stumpwm:current-screen))))))))

(defun mode-line-format ()
  (mode-line-formatters *stumpwm-modeline-frame*))

(defun set-mode-line-format (list)
  (setf (mode-line-formatters *stumpwm-modeline-frame*) list))

(defmethod clime:find-frame-type ((frame mode-line)) 
  "this method sets the window type via clim-clx::adopt-frame as defined below."
  :dock)

(defvar *mode-line-display-function* 'display-mode-line-as-table)

(defun display-mode-line (frame pane)
  (with-text-style (pane ;; (frame-text-style frame)
                    ;; TODO: We may need to 
                    ;; (make-text-style "DejaVu Sans Mono" "Book" 14)
                    (make-text-style nil nil nil) ; use default text style
                    )
    (funcall *mode-line-display-function* frame pane)))

(defun display-mode-line-as-table (frame pane)
  (with-table (pane)
    (dolist (line (mode-line-formatters frame))
      (with-table-row ()
        (funcall (car line) frame pane (cdr line))))))

(defun display-mode-line-as-text (frame pane)
  (dolist (line (mode-line-formatters frame))
    (funcall (car line) frame pane (cdr line))))

;; Glue between this and StumpWM

(defun update-mode-line ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-refresh))))

(defun update-mode-line-hanger (&rest ignore)
  (declare (ignore ignore))
  (update-mode-line))

(defun app-main (&optional (head (stumpwm:current-head)))
  (let* ((total-width (stumpwm::head-width head))
         (frame (make-application-frame 'mode-line
                                        :left 0
                                        :top 0
                                        :height 10
                                        :width total-width
                                        :head-width total-width)))
    (setf *stumpwm-modeline-frame* frame)
    (stumpwm:add-hook stumpwm:*post-command-hook* 'update-mode-line-hanger)
    (sb-thread:make-thread
     (lambda () 
       (run-frame-top-level frame))
     :name "CLIM-MODE-LINE")))

(defun debug-kill-restart ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-quit)))
  (setf *stumpwm-modeline-frame* nil)
  (stumpwm:run-commands "restart-soft"))

(defun debug-kill-restart-hard ()
  (when *stumpwm-modeline-frame*
    (execute-frame-command *stumpwm-modeline-frame* '(com-quit)))
  (setf *stumpwm-modeline-frame* nil)
  (stumpwm:run-commands "restart-hard"))

(defun redisp (frame)
  (sb-thread:with-mutex ((mode-line-mutex frame))
    (redisplay-frame-panes frame :force-p t)
    (stumpwm:call-in-main-thread
     (lambda ()
       (sb-thread:with-mutex ((mode-line-mutex frame))
         (let* ((sheet (frame-top-level-sheet frame))
                (space (compose-space sheet))
                (width (space-requirement-width space))
                (height (space-requirement-height space)))
           (move-and-resize-sheet sheet 0 0 width height))
         (mapcar 'stumpwm::resize-mode-line
                 stumpwm::*mode-lines*)
         (mapcar (lambda (group)
                   (mapcar (lambda (head)
                             (stumpwm::group-sync-head
                              group head))
                           (stumpwm::group-heads group)))
                 (stumpwm::screen-groups
                  (stumpwm:current-screen))))))))
