(in-package #:clim-mode-line)

(defmacro do-list-with-interspersed-element
    ((var list &body interspersed-forms) &body body)
  (alexandria:with-gensyms (initial rest hold cont tmp)
    `(flet ((,cont (,var)
              ,@body))
       (let* ((,hold ,list)
              (,initial (car ,hold))
              (,rest (cdr ,hold)))
         (,cont ,initial)
         (dolist (,tmp ,rest)
           ,@interspersed-forms
           (,cont ,tmp))))))

(defvar *stumpwm-modeline-frame* nil
  "Hold the single mode line")

(define-application-frame mode-line ()
  ((formatters :initform (list (list 'format-groups 'format-bar 'format-windows))
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

(defmacro with-inverted-ink ((pane &rest drawing-options
                              &key (ink '+flipping-ink+) fg-ink bg-ink
                              &allow-other-keys)
                             &body body)
  (let ((p (gensym "PANE")))
    `(let ((,p ,pane))
       (surrounding-output-with-border (,p :ink ,(or bg-ink ink)
                                           :filled t
                                           :move-cursor nil)
         (with-drawing-options (,p :ink ,(or fg-ink ink) ,@drawing-options)
           ,@body)))))

(defvar *mode-line-display-function* 'display-mode-line-as-table)

(defun display-mode-line (frame pane)
  (with-text-style (pane ;; (frame-text-style frame)
                    ;; TODO: We may need to 
                    ;; (make-text-style "DejaVu Sans Mono" "Book" 14)
                    (make-text-style nil nil nil) ; use default text style
                    )
    (funcall *mode-line-display-function* frame pane)))

(defmacro with-table ((pane &rest options) &body body)
  ;; Set up a table, and make sure we can communicate it to functions further down
  ;; the call stack.
  `(slim:with-table (,pane ,@options)
     (let ((*display-as-table* t)
           (*display-style* :table))
       ,@body)))

(defmacro with-table-row ((&rest options) &body body)
  ;; Defined because we want a consistent syntax. Plus then we can make our own
  ;; changes without anyone needing to rewrite anything (well, unless we add
  ;; required arguments)
  (declare (ignore options))
  `(slim:row ,@body))

(defmacro with-cell ((&optional (pane 'slim:*pane*) &rest options) &body body)
  (declare (ignore options))
  (alexandria:with-gensyms (cont)
    `(flet ((,cont ()
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       (case *display-style*
         ((:text) (funcall #',cont))
         ((:table) (formatting-cell (,pane :align-x *align-x*)
                     (funcall #',cont)))))))

(defun display-mode-line-as-table (frame pane)
  (with-table (pane)
    (dolist (line (mode-line-formatters frame))
      (with-table-row ()
        (funcall (car line) frame pane (cdr line))))))

(defmacro with-undrawn-output-record ((stream) &body body)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (with-output-recording-options (,s :draw nil :record t)
         (with-new-output-record (,s)
           ,@body)))))

(defmacro with-output-record-bounds ((x y width height) record &body body)
  (alexandria:with-gensyms (rec)
    `(let ((,rec ,record))
       (multiple-value-bind (,x ,y) (output-record-position ,rec)
         (declare (ignorable ,x ,y))
         (multiple-value-bind (,width ,height) (bounding-rectangle-size ,rec)
           (declare (ignorable ,width ,height))
           ,@body)))))

(defmacro with-right-alignment ((frame pane) &body body)
  (alexandria:with-gensyms (stream width record)
    `(let* ((,stream ,pane)
            (,record
              (with-output-recording-options (,stream :draw nil :record t)
                (with-new-output-record (,stream)
                  ,@body)))
            (,width (mode-line-head-width ,frame)))
       (multiple-value-bind (x y) (output-record-position ,record)
         (declare (ignore x))
         (multiple-value-bind (w h) (bounding-rectangle-size ,record)
           (declare (ignore h))
           (setf (output-record-position ,record)
                 (values (- ,width w) y))))
       (tree-recompute-extent ,record)
       (replay ,record ,stream))))

(defun display-mode-line-as-text (frame pane)
  (dolist (line (mode-line-formatters frame))
    (with-new-output-record )
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
                                        :head-width total-width)
                ;; (or *stumpwm-modeline-frame*
                ;;     )
                ))
    ;; (unless *stumpwm-modeline-frame*
    (setf *stumpwm-modeline-frame* frame)
    ;; )
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
