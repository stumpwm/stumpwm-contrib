;;;; sensors.lisp

(in-package #:sensors)

(defvar *refresh-time* 30
  "Time in seconds between updates of sensors information.")

(defvar *temp-regex* "(?<=\\+).*[0-9]+(?=\\..*)"
  "A regex that captures all temperatures.")

(defvar *fan-regex* "(?<=\\:).*?(?=RPM)"
  "A regex that captures all fans.")

(defvar *red-above-temp* 60
  "Temperature to turn red at.")

(defvar *yellow-above-temp* 50
  "Temperature to turn yellow at.")

(defvar *display-above-temp* 40
  "Temperature to start displaying at.")

(defvar *red-above-rpm* 4000
  "Fan RPM to turn red at.")

(defvar *yellow-above-rpm* 3000
  "Fan RPM to turn yellow at.")

(defvar *display-above-rpm* 2000
  "Fan RPM to start displaying at.")

(defvar *ignore-below* 20
  "Ignore temperatures below this temperature when calculating average.")

(defun sensors-as-ints (output regex)
  (let ((strings (ppcre:all-matches-as-strings regex output)))
    (mapcan ;; https://stackoverflow.com/a/13269952
     (lambda (s)
       (let ((i (parse-integer (remove-if #'alpha-char-p s) :junk-allowed t)))
	 ;; low readings can skew the average too much
	 (if (< *ignore-below* i)
	     (list i))))
     strings)))

(defun get-colors (value
		   &optional
		     (high *red-above-temp*)
		     (mid *yellow-above-temp*)
		     (low *display-above-temp*))
  (cond ((< high value) (concat "^1*" (write-to-string value)))
	((< mid value) (concat "^3*" (write-to-string value)))
	((< low value) (write-to-string value))
	(t nil)))

(defun get-sensors ()
  (let* ((output (run-shell-command "sensors" t))
	 (temps (sensors-as-ints output *temp-regex*))
	 (fans (sensors-as-ints output *fan-regex*))
	 (max-temp (get-colors (reduce #'max temps)))
	 (avg-temp (get-colors
		    (handler-case
			(floor (apply #'+ temps) (length temps))
		      (division-by-zero () 0))))
	 (avg-rpm (get-colors
		   (handler-case
		       (floor (apply #'+ fans) (length fans))
		     (division-by-zero () 0))
		   *red-above-rpm* *yellow-above-rpm* *display-above-rpm*)))
    (concat
     (if max-temp (concat max-temp (string (code-char 176)) "C^n"))
     (if avg-temp (concat " " avg-temp (string (code-char 176)) "C^n"))
     (if avg-rpm (concat " " avg-temp "RPM^n")))))

(defcommand sensors () ()
  (message (get-sensors)))

;; pinched from battery portable code
(let ((next 0)
      (last-value ""))
  (defun fmt-sensors (ml)
    (declare (ignore ml))
    (let ((now (get-universal-time)))
      (when (< now next)
	(return-from fmt-sensors last-value))
      (setf next (+ now *refresh-time*)))
    (setf last-value (get-sensors))))

(add-screen-mode-line-formatter #\S #'fmt-sensors)
