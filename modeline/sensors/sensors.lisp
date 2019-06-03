;;;; sensors.lisp

(in-package #:sensors)

(defvar *refresh-time* 30
  "Time in seconds between updates of sensors information.")

(defvar *cpu-regex* "(?<=\\+).*[0-9]+.*C(?=.*\\()"
  "A regex that captures all temperatures.")

(defvar *fan-regex* "(?<=\\:).*[0-9]+.*RPM"
  "A regex that captures all fans.")

(defun get-sensor (output start-string end-string upper-bound lower-bound)
  "Rips out the the value of a sensors row from the OUTPUT from the sensors
   command, using a START-STRING and END-STRING that mark the place of the
   relevant sensor information. Adds color depending on it's value when
   converted to an int, specified by the UPPER-BOUND and LOWER-BOUND."
  (let* ((start (search start-string output))
	 (end (search end-string output))
	 (sensor (subseq output start end))
	 ;; Get rid of the junk + and : characters from sensors' output
	 (sensor (string-trim (concat start-string "+" " " ":" ) sensor))
	 (int (parse-integer (remove-if #'alpha-char-p sensor) :junk-allowed t)))
    (cond ((< upper-bound int) (concat "^1*" sensor "^n"))
	  ((< lower-bound int) (concat "^3*" sensor "^n"))
	  (t sensor))))

(defun get-sensors (&optional as-string)
  "Gets a large string back from running sensors in the shell, and then parses
   out a CPU temperature value and fan RPM using get-cpu-temp and get-fan-rpm.
   Takes optional AS-STRING boolean which if true returns output as formatted
   string."
  (let* ((output (run-shell-command "sensors" t))
	 (cpu-temp (get-sensor output "Package id 0:" "(high" 60 50))
	 (fan-rpm (get-sensor output "Exhaust" "(min" 3500 2500)))
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
