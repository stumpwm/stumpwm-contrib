;;;; cycle-mru.lisp

(in-package #:cycle-mru)

(defun get-milliseconds () ()
       "Get timestamp in milliseconds."
       (floor (* 1000 (/ (get-internal-real-time)
			 internal-time-units-per-second))))

(defun mru-new-window (win) ()
       "Function called by new-window-hook. Updates mru-list."
       (push (window-number win)  *mru-list*))

(defun mru-destroy-window (win) ()
       "Function called by destroy-window-hook. Deletes window number from
	mru-list."
       (setf *mru-list* (delete (window-number win) *mru-list*)))

(defun mru-focus-window (new-win old-win) ()
       "Function called by focus-window-hook. Updates mru-list."
       (when new-win
	 (setf *mru-list* (delete (window-number new-win) *mru-list*)))
       (when old-win
	 (push (window-number old-win) *mru-list*)))

(defun mru-focus-group (to-group from-group) ()
       "Handling group switching."
       (setf *mru-list* (mapcar 'window-number (group-windows to-group))))

(setf *new-window-hook* (list 'mru-new-window))
(setf *destroy-window-hook* (list 'mru-destroy-window))
(setf *focus-window-hook* (list 'mru-focus-window))
(setf *focus-group-hook* (list 'mru-focus-group))

(defvar *mru-list* (make-list 0))
(defvar *mru-cycle* (make-list 0))
(defvar *mru-timeout* 500) ; in milliseconds
(defvar *mru-index* 0)     ; in milliseconds
(defvar *mru-last-call* (get-milliseconds))

(defcommand cycle-mru () ()
  "Focus next windows according to most recently used order."
  (cond
    ;; if we're still in timeout
    ((< (- (get-milliseconds) *mru-last-call*) 500)
     ;; cycle through windows
     (setf *mru-index* (mod (1+ *mru-index*) (list-length *mru-cycle*)))
     (select-window-by-number (nth *mru-index* *mru-cycle*)))
    ;; else start anew
    (
     (setf *mru-index* 1)
     (setf *mru-cycle* (copy-list *mru-list*))
     ;; if we've focussed a frame without window, current-window will return nil
     ;; => select last used
     (cond
       ((not (current-window))
	(select-window-by-number (first *mru-cycle*)))
       (
	(push (window-number (current-window)) *mru-cycle*)
	(select-window-by-number (second *mru-cycle*))))))

  ;; update timestamp
  (setf *mru-last-call* (get-milliseconds)))
