;;;; sensors.lisp

(in-package #:sensors)

(defvar *refresh-time* 30
  "Time in seconds between updates of sensors information.")

(defvar *temp-regex* "(?<=\\+).*[0-9]+(?=\\..*)"
  "A regex that captures all temperatures.")

(defvar *fan-regex* "(?<=\\:).*?(?=RPM)"
  "A regex that captures all fans.")

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

(defun get-colors (value high mid low)
  (cond ((< high value) (concat "^1*" (write-to-string value)))
	((< mid value) (concat "^3*" (write-to-string value)))
	((< low value) (write-to-string value))
	(t nil)))

(defun get-sensors (&optional as-string)
  (let* ((output (run-shell-command "sensors" t))
	 (temps (sensors-as-ints output *temp-regex*))
	 (fans (sensors-as-ints output *fan-regex*))
	 (max-temp (get-colors (reduce #'max temps) 60 50 40))
	 (avg-temp (get-colors
		    (handler-case
			(floor (apply #'+ temps) (length temps))
		      (division-by-zero () 0))
		    60 50 40))
	 (avg-rpm (get-colors
		   (handler-case
		       (floor (apply #'+ fans) (length fans))
		     (division-by-zero () 0))
		   4000 3000 2000)))
    (if as-string
	(concat
	 (if max-temp (concat max-temp (string (code-char 176))) "C^n ")
	 (if avg-temp (concat avg-temp (string (code-char 176))) "C^n ")
	 (if avg-rpm (concat avg-temp "RPM^n")))
	(list max-temp avg-temp avg-rpm))))

(defcommand sensors () ()
  (let* ((s (get-sensors))
	 (max-temp (nth 0 s))
	 (avg-temp (nth 1 s))
	 (avg-rpm (nth 2 s)))
    (message (concat
	      "^B^5MAXIMUM TEMPERATURE: ^n~a" (string (code-char 176)) "C~%"
	      "^B^5AVERAGE TEMPERATURE: ^n~a" (string (code-char 176)) "C~%"
	      "^B^5AVERAGE FAN SPEED: ^n~a RPM")
	     max-temp avg-temp avg-rpm)))

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
