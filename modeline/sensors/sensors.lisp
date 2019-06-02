;;;; sensors.lisp

(in-package #:sensors)

(defvar *refresh-time* 30
  "Time in seconds between updates of sensors information.")

(defun get-cpu-temp (output)
  "Rips out the the value of the Package id 0 row from the OUTPUT of the sensors
   command. Adds color depending on it's value when converted to an int."
  (let* ((start (search "Package id 0:" output))
	 (end (search "(high" output))
	 (cpu-temp (subseq output start end))
	 (cpu-temp (string-trim "Package id 0: +" cpu-temp))
	 (int (parse-integer (remove-if #'alpha-char-p cpu-temp) :junk-allowed t)))
    (cond ((< 60 int) (concat "^1*" cpu-temp "^n"))
	  ((< 50 int) (concat "^3*" cpu-temp "^n"))
	  (t cpu-temp))))

(defun get-fan-rpm (output)
  "Rips out the value of the exhaust row from the OUTPUT of the sensors
  command. Adds color depending on it's value when converted to an int."
  (let* ((start (search "Exhaust" output))
	 (end (search "(min" output))
	 (fan-rpm (subseq output start end))
	 (fan-rpm (string-trim "Exhaust :" fan-rpm))
	 (int (parse-integer (remove-if #'alpha-char-p fan-rpm) :junk-allowed t)))
    (cond ((< 3500 int) (concat "^1*" fan-rpm "^n"))
	  ((< 2500 int) (concat "^3*" fan-rpm "^n"))
	  (t fan-rpm))))

(defun get-sensors (&optional as-string)
  "Gets a large string back from running sensors in the shell, and then parses
   out a CPU temperature value and fan RPM using get-cpu-temp and get-fan-rpm.
   Takes optional AS-STRING boolean which if true returns output as formatted
   string."
  (let* ((output (run-shell-command "sensors" t))
	 (cpu-temp (get-cpu-temp output))
	 (fan-rpm (get-fan-rpm output)))
    (if as-string
	(concat cpu-temp " " fan-rpm)
	(list cpu-temp fan-rpm))))

(defcommand sensors () ()
	    (let* ((my-sensors (get-sensors))
		   (cpu-temp (nth 0 my-sensors))
		   (fan-rpm (nth 1 my-sensors)))
	      (message "^B^5CPU: ^n~a~%^B^5FAN: ^n~a" cpu-temp fan-rpm)))

;; pinched from battery portable code
(let ((next 0)
      (last-value ""))
  (defun fmt-sensors (ml)
    (declare (ignore ml))
    (let ((now (get-universal-time)))
      (when (< now next)
	(return-from fmt-sensors last-value))
      (setf next (+ now *refresh-time*)))
    (setf last-value (get-sensors t))))

(add-screen-mode-line-formatter #\S #'fmt-sensors)
