(in-package :stump-radio)

;; radio stations

(defvar *stations*
  '((:|Le DJAM| . "http://www.djamradio.com/sound")
    (:|FluxFM| . "http://streams.fluxfm.de/live/mp3-320/audio/")
    (:|1Live| . "https://wdr-1live-live.icecastssl.wdr.de/wdr/1live/live/mp3/128/stream.mp3"))
  "association list of radio stations, key is a name, value is a playable URL of the radio station")

(defun add-station (name url)
  (setf *stations* (acons name url *stations*)))

(defun remove-station (name)
  (setf *stations* (remove name *stations* :key #'car)))

(defun list-stations ()
  (mapcar #'car *stations*))

(defun clear-stations ()
  "clear all stations in case you want to get rid of the defaults"
  (setf *stations* nil))

(defun assert-stations ()
  "make sure *stations* is not empty; to be used in functions that try to access *stations*"
  (assert (consp *stations*) (*stations*) "no radio stations in *stations*"))

(defun next-radio-station ()
  (assert-stations)
  (setf (cdr (last *stations*)) (list (car *stations*))
        *stations* (cdr *stations*))
  (car *stations*))

(defun previous-radio-station ()
  (assert-stations)
  (setf *stations* (cons (car (last *stations*))  *stations*)
        (cdr (last *stations* 2)) nil)
  (car *stations*))

;; radio playing and control

(defvar *radio* nil
  "holds the process structure of mplayer when a radio is running (or at least we think it is running),
  NIL if not playing anything (as it was not yet ever playing or we properly shut it down)")

(defun radio-running-p ()
  (and *radio*
       ;; don't test for :running, but for (not :exited),
       ;; as the process might also be in :stopped or :signaled
       (not (eq (sb-ext:process-status *radio*) :exited))))

(defvar *sent-termination-signal* nil
  "used to detect that a status change to :exited was caused by
  our own intentional process termination")

(defun radio-status-change (process)
  (message (if (and (eq (sb-ext:process-status process) :exited)
                    *sent-termination-signal*)
               "Radio stopped."
               (format nil "Radio status changed to ~a.~%(Process ID: ~a)"
                       (sb-ext:process-status process)
                       (sb-ext:process-pid process)))))

(defcommand radio-start () ()
  "start radio if not running"
  (assert-stations)
  (if (radio-running-p)
      (message "Warning: radio already running, not starting.")
      (destructuring-bind (name . url)
          (car *stations*)
        (message (format nil "Starting ~a radio..." name))
        (setf *sent-termination-signal* nil
              *radio*
              (sb-ext:run-program "mplayer" (list url)
                                  :search t
                                  :wait nil
                                  :status-hook #'radio-status-change)))))
(defcommand radio-stop () ()
  "stop radio if running"
  (if (not (radio-running-p))
      (message "Warning: radio not running, not stopping.")
      (progn
        (message "Stopping radio...")
        (setf *sent-termination-signal* t)
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

(defcommand radio-previous-station () ()
  "switch to previous radio station and play that"
  (previous-radio-station)
  (radio-force-restart))

(defcommand radio-list-stations () ()
  "list radio stations"
  (message (format nil "radio stations: ~{~&  ~a~^,~}" (list-stations))))
