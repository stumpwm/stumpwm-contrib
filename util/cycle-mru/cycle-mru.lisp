;;;; cycle-mru.lisp

(in-package #:cycle-mru)

(defun get-milliseconds () ()
       "Get timestamp in milliseconds."
       (floor (* 1000 (/ (get-internal-real-time)
			 internal-time-units-per-second))))

(defvar *mru-last-call* (get-milliseconds))
(defvar *mru-list* (make-list 0))
(defvar *mru-cycle* (make-list 0))
(defvar *mru-timeout* 500) ; in milliseconds
(defvar *mru-index* 0)     ; in milliseconds

(defun mru-new-window (win) ()
       "Function called by new-window-hook. Updates mru-list."
       (push (stumpwm:window-number win) *mru-list*))

(defun mru-destroy-window (win) ()
       "Function called by destroy-window-hook. Deletes window number from
	mru-list."
       (setf *mru-list* (delete (stumpwm:window-number win) *mru-list*)))

(defun mru-focus-window (new-win old-win) ()
       "Function called by focus-window-hook. Updates mru-list."
       (when new-win
	 (setf *mru-list* (delete (stumpwm:window-number new-win) *mru-list*)))
       (when old-win
	 (push (stumpwm:window-number old-win) *mru-list*)))

(setf stumpwm:*new-window-hook* (list 'mru-new-window))
(setf stumpwm:*destroy-window-hook* (list 'mru-destroy-window))
(setf stumpwm:*focus-window-hook* (list 'mru-focus-window))

(stumpwm:defcommand cycle-mru () ()
  "Focus next windows according to most recently used order."
  (cond
    ;; if we're still in timeout
    ((< (- (get-milliseconds) *mru-last-call*) 400)
     ;; cycle through windows
     (setf *mru-index* (mod (1+ *mru-index*) (list-length *mru-cycle*)))
     (stumpwm:select-window-by-number (nth *mru-index* *mru-cycle*)))
    ;; else start anew
    ((setf *mru-index* 1)
     (setf *mru-cycle* (copy-list *mru-list*))
     ;; if we've focussed a frame without window, current-window will
     ;; return nil => select last used
     (cond
       ((not (stumpwm:current-window))
	(stumpwm:select-window-by-number (first *mru-cycle*)))
       ((push (stumpwm:window-number (stumpwm:current-window)) *mru-cycle*)
	(stumpwm:select-window-by-number (second *mru-cycle*))))))

  ;; update timestamp
  (setf *mru-last-call* (get-milliseconds)))
