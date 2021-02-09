;;;; mode-line-formatters.lisp
(in-package :clim-mode-line)

;;;; Formatters for clim-mode-line are markedly different from the formatters
;;;; for the stumpwm mode line. While I think a translator could be written,
;;;; at the moment we will just focus on the formatters native to clim-mode-line


;;;; Default formatters are functions that take a frame and a pane, and write output
;;;; via calls to slim:cell. slim:cell is the only thing that should be used, as by
;;;; default the functions will be wrapped in calls to slim:with-table, slim:row,
;;;; and slim:col.


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

(defun format-nil (frame pane)
  (declare (ignore frame))
  (slim:cell (format pane "NIL")))

(let ((msg ""))
  (defun format-msg (frame pane)
    (declare (ignore frame))
    (slim:cell (format pane "~a" msg)))
  (defun set-msg (m)
    (setf msg m)))

(defparameter *selected* ;; (lambda (thing) thing)
  nil
  "every top priority click command must check if this is non-nil. if it is. they must call it and bind it to
the value return by itself.")

;;; Group formatting

(define-gesture-name :right-click :pointer-button-press (:right))
(define-gesture-name :left-ctrl-click :pointer-button-press (:left :control))
(define-gesture-name :left-meta-click :pointer-button-press (:left :meta))

;;; We need a presentation type for groups.
(define-presentation-type stumpwm-group-presentation ())

;; (define-clim-mode-line-command (com-move-window-to-group-2arg)
;;     ((window stumpwm::window) (group stumpwm::group))
;;   (stumpwm::move-window-to-group window group))

(define-drag-and-drop-translator drag-window (stumpwm-window-presentation command stumpwm-group-presentation
									  clim-mode-line
									  :gesture :left-meta-click)
				 (object cmd destination)
  `(stumpwm::message "~a" '(,object ,cmd ,destination)))

;; (define-drag-and-drop-translator drag-group (stumpwm-group-presentation command stumpwm-window-presentation
;; 									  clim-mode-line
;; 									  :gesture :left-meta-click
;; 									  ;; :priority 9
;; 									)
;; 				 (object destination)
;;   `(com-move-window-to-group-2arg ,destination ,object))

;; (define-drag-and-drop-translator stumpwm-window-to-group-drag-and-drop-translator
;;     (stumpwm-window-presentation command stumpwm-group-presentation clim-mode-line
;; 				 :documentation "Move Window to Group"
;; 				 :gesture :left-ctrl-click)
;;     (window group)
;;   `(com-move-window-to-group-2arg ,window ,group))

;;; now we define commands and their presentation to command translators.
(define-clim-mode-line-command (com-switch-to-group++)
    ((group stumpwm::group))
  "this command must do its own checking to see if it is supposed to move a window to this group... 
there must be a better way to do this... this is rediculous!!"
  (if (typep (move-window-to-group-item) 'stumpwm::window)
      (com-move-window-to-group group)
      (stumpwm::switch-to-group group)))

(define-presentation-to-command-translator group-switch
    (stumpwm-group-presentation com-switch-to-group++ clim-mode-line
     :gesture :select
     :priority 9
     :documentation "Switch to group")
    (group)
  (list group))

;; (define-presentation-method )

(define-clim-mode-line-command (com-kill-group :name t)
    ((group t))
  (when (typep group 'stumpwm::group)
    (let* ((groups (stumpwm::screen-groups (stumpwm::group-screen group)))
	   (to-group (or (stumpwm::next-group group (stumpwm::non-hidden-groups groups))
			 (stumpwm::next-group group groups))))
      (when to-group
	(stumpwm::kill-group group to-group)))))

(define-presentation-to-command-translator group-kill
    (stumpwm-group-presentation com-kill-group clim-mode-line
     :priority 0)
    (group)
  (list group))

(define-presentation-to-command-translator group-move-window-to-group
    (stumpwm-group-presentation com-move-window-to-group clim-mode-line
     :priority 8)
    (group)
  (list group))

(let ((item nil))
  (define-clim-mode-line-command (com-move-window-to-group)
      ((group stumpwm::group))
    (if (typep item 'stumpwm::window)
	(prog1 (stumpwm::move-window-to-group item group)
	  (setf item nil))
	(setf item group)))
  (defun move-window-to-group-item (&optional (set-to nil set-to-provided-p))
    (when set-to-provided-p (setf item set-to))
    item)
  (define-clim-mode-line-command (com-to-group-move-window)
      ((window stumpwm::window))
    (if (typep item 'stumpwm::group)
	(prog1 (stumpwm::move-window-to-group window item)
	  (setf item nil))
	(setf item window))))

(defgeneric get-presentation-using-selected (presentation-type-symbol)
  (:method (sym) sym)
  (:method ((sym (eql 'stumpwm-group-presentation)))
    (cond ((typep *selected-item* 'stumpwm::window)
	   'stumpwm-move-window-to-group-presentation)
  	  (t sym))))

;; (with-output-to-presentation (pane group (dispatch-on-selected 'stumpwm-group-presentation)))

(defun format-groups (frame pane)
  (declare (ignore frame))
  (slim:cell (format pane "["))
  (loop for group in (stumpwm::sort-groups (stumpwm:current-screen))
	do (slim:cell
	    (if (equal group (stumpwm:current-group))
		(with-output-as-presentation (pane group ;; 'stumpwm-current-group-presentation
						   'stumpwm-group-presentation
						   :single-box t)
		  (with-drawing-options (pane :ink +red+)
		    (format pane "~a" (stumpwm:group-name group))))
		(with-output-as-presentation (pane group 'stumpwm-group-presentation
						   ;; (get-presentation-using-selected )
						   :single-box t)
		  (format pane "~a" (stumpwm:group-name group))))))
  (slim:cell (format pane "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window Formatting ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-type stumpwm-window-presentation ())
(define-presentation-to-command-translator window-move-window-to-group
    (stumpwm-window-presentation com-to-group-move-window clim-mode-line
     :priority 8)
    (window)
  (list window))

(define-presentation-to-command-translator focus-window
    (stumpwm-window-presentation com-focus-window clim-mode-line
     :priority 9)
    (window)
  (list window))
(define-clim-mode-line-command (com-focus-window)
    ((window stumpwm::window))
  (when (typep window 'stumpwm::window)
    (stumpwm::focus-all window)))

(defun format-windows (frame pane)
  (declare (ignore frame))
  (slim:cell (format pane "["))
  (loop for window in (stumpwm::sort-windows-by-number (stumpwm::group-windows (stumpwm::current-group)))
	do (slim:cell
	     (if (equal (stumpwm::current-window) window)
		 (with-drawing-options (pane :ink +red+)
		   (with-output-as-presentation (pane window 'window ;; 'stumpwm-window-presentation
						      :single-box t)
		     (format pane "~a" (stumpwm::format-expand stumpwm:*window-formatters*
							       stumpwm:*window-format*
							       window))))
		 (with-output-as-presentation (pane window 'window ;; 'stumpwm-window-presentation
						    :single-box t)
		   (format pane "~a" (stumpwm::format-expand stumpwm:*window-formatters*
							     stumpwm:*window-format*
							     window))))))
  (slim:cell (format pane "]")))


;; (define-presentation-to-command-translator focus-window
;;     (stumpwm-window-presentation ))

