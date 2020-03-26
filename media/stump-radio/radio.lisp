(in-package :stump-radio)

(defvar *radio* nil)

(defparameter *stations* '((:|Le DJAM| . "http://www.djamradio.com/sound")
			 (:|FluxFM| . "http://streams.fluxfm.de/live/mp3-320/Android/")
			 (:|1Live| . "http://1liveuni-lh.akamaihd.net/i/1LIVE_HDS@179577/index_1_a-p.m3u8?sd=10"))
  "association list of radio stations, key is a name, value is a playable URL of the radio station")

(defun add-station (name url)
  (setf *stations* (acons name url *stations*)))

(defun remove-station (name)
  (setf *stations* (remove name *stations* :key #'car)))

(defun list-stations ()
  (mapcar #'car *stations*))

(defun next-radio-station ()
  (setf (cdr (last *stations*)) (list (car *stations*))
	*stations* (cdr *stations*)))

(defun radio-status-change (process)
  (message (format nil "radio status changed to: ~a (PID: ~a)"
						    (sb-ext:process-status process)
						    (sb-ext:process-pid process))))

(defun radio-running-p ()
  (and *radio*
       ;; don't test for :running, but for (not :exited),
       ;; as the process might also be in :stopped or :signaled
       (not (eq (sb-ext:process-status *radio*) :exited))))

(defcommand radio-start () ()
  "start radio if not running"
  (if (radio-running-p)
      (message "Warning: radio already running, not starting.")
      (destructuring-bind (name . url)
	  (car *stations*)
	(message (format nil "Starting ~a radio..." name))
	(setf *radio*
	      (sb-ext:run-program "/usr/bin/mplayer" (list url)
				  :wait nil
				  :status-hook #'radio-status-change)))))
(defcommand radio-stop () ()
  "stop radio if running"
  (if (not (radio-running-p))
      (message "Warning: radio not running, not stopping.")
      (progn
	(message "Stopping radio...")
	(sb-ext:process-close *radio*) ;; close (to supress status changed hook)
	(sb-ext:process-kill *radio* 15) ;; SIGTERM
	(setf *radio* nil))))

(defcommand radio-toggle-playback () ()
  "stop radio if running and start playing if not"
  (if (radio-running-p)
      (radio-stop)
      (radio-start)))

(defcommand radio-force-restart () ()
  "stop current radio and start playing again
 (use for network problems or after suspend)"
  (radio-stop)
  (radio-start))

(defcommand radio-next-station () ()
  "switch to next radio station and play that"
  (next-radio-station)
  (radio-force-restart))

(defcommand radio-list-stations () ()
  "list radio stations"
  (message (format nil "radio stations: ~{~a~^, ~}" (list-stations))))
