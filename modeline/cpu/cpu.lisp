;;;; cpu.lisp

(in-package #:cpu)

;;; "cpu" goes here. Hacks and glory await!

;;; CPU formatters for the mode-line
;;;
;;; Copyright 2007 Anonymous Coward, Jonathan Moore Liles;
;;;           2021 Benjamin Slade. 
;;;
;;; Maintainer: Julian Stecklina
;;;

;; Install formatters.
(add-screen-mode-line-formatter #\C 'cpu-modeline)

;; Defaults arguments for fmt-cpu-usage-bar
(defvar *cpu-usage-bar-width* 10)
(defvar *cpu-usage-bar-full* #\#)
(defvar *cpu-usage-bar-empty* #\:)

(defvar *prev-user-cpu* 0)
(defvar *prev-sys-cpu* 0)
(defvar *prev-idle-cpu* 0)
(defvar *prev-iowait* 0)
(defvar *prev-result* '(0 0 0))
(defvar *prev-time* 0)

;; Defaults for medium, high, critical temperature colouring
(defvar *cpu-temp-med* 65)
(defvar *cpu-temp-hi* 75)
(defvar *cpu-temp-crit* 90)


(defvar *cpu-usage-modeline-fmt* "CPU: ^[~A~3D%^] "
  "The default formatting for CPU usage")

;; More or less yanked from the wiki.
(defun current-cpu-usage ()
  "Return the average CPU usage since the last call.  First value is percent
of CPU in use.  Second value is percent of CPU in use by system processes.
Third value is percent of time since last call spent waiting for IO (or 0 if
not available). Don't make calculation more than once a second."
  (let ((cpu-result 0)
        (sys-result 0)
        (io-result nil)
        (now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *prev-time*) 1)
      (setf *prev-time* now)
      (with-open-file (in #P"/proc/stat" :direction :input)
        (read in)
        (let* ((norm-user (read in))
               (nice-user (read in))
               (user (+ norm-user nice-user))
               (sys (read in))
               (idle (read in))
               (iowait (or (ignore-errors (read in)) 0))
               (step-denom (- (+ user sys idle iowait)
                              (+ *prev-user-cpu* *prev-sys-cpu* *prev-idle-cpu* *prev-iowait*))))
          (unless (zerop step-denom)    ; This should never happen, but in some
                                        ; environments it does.
            (setf cpu-result (/ (- (+ user sys)
                                   (+ *prev-user-cpu* *prev-sys-cpu*))
                                step-denom)
                  sys-result (/ (- sys *prev-sys-cpu*)
                                step-denom)
                  io-result (/ (- iowait *prev-iowait*)
                               step-denom)
                  *prev-user-cpu* user
                  *prev-sys-cpu* sys
                  *prev-idle-cpu* idle
                  *prev-iowait* iowait
                  *prev-result* (list cpu-result sys-result io-result)))))))
  (apply 'values *prev-result*))

(defun fmt-cpu-usage ()
  "Returns a string representing current the percent of average CPU
  utilization."
  (let ((cpu (truncate (* 100 (current-cpu-usage)))))
    (format nil *cpu-usage-modeline-fmt* (bar-zone-color cpu) cpu)))

(defun fmt-cpu-usage-bar (ml &optional (width *cpu-usage-bar-width*) (full *cpu-usage-bar-full*) (empty *cpu-usage-bar-empty*))
  "Returns a coloured bar-graph representing the current percent of average CPU
utilization."
  (declare (ignore ml))
  (let ((cpu (truncate (* 100 (current-cpu-usage)))))
    (stumpwm::bar cpu width full empty)))

(defun get-proc-file-field (fname field)
  (with-open-file (s fname :if-does-not-exist nil) ;
    (if s
        (do ((line (read-line s nil nil) (read-line s nil nil)))
            ((null line) nil)
          (let ((split (cl-ppcre:split "\\s*:\\s*" line)))
            (when (string= (car split) field) (return (cadr split)))))
        "")))

(defun get-proc-file-fields (fname field)
  (let ((ret ()))
    (with-open-file (s fname :if-does-not-exist nil) ;
      (if s
          (do ((line (read-line s nil nil) (read-line s nil nil)))
              ((null line) nil)
            (let ((split (cl-ppcre:split "\\s*:\\s*" line)))
              (when (string= (car split) field) (setq ret (cons (cadr split) ret)))))
          ""))
    ret))

(defun fmt-mhz (mhz)
  (if (>= mhz 1000)
        (format nil "~,2FGHz" (/ mhz 1000))
        (format nil "~DMHz" mhz)))

(defun fmt-cpu-freq ()
  "Returns a string representing the current CPU frequency (especially useful for laptop users.)"
  (let ((mhz (parse-integer (get-proc-file-field "/proc/cpuinfo" "cpu MHz")
                            :junk-allowed t)))
    (fmt-mhz mhz)))

(defun fmt-cpu-freq-range ()
  (let ((freqs (get-proc-file-fields "/proc/cpuinfo" "cpu MHz")))
    (if freqs
	(let ((maxmhz (parse-integer (car freqs) :junk-allowed t))
	      (minmhz (parse-integer (car freqs) :junk-allowed t)))
	  (dolist (mhz freqs)
	    (let ((m (parse-integer mhz :junk-allowed t)))
	      (when (> m maxmhz) (setq maxmhz m))
	      (when (< m minmhz) (setq minmhz m))))
	  (format nil "~A-~A" (fmt-mhz minmhz) (fmt-mhz maxmhz)))
	"")))

(defvar *acpi-thermal-zone*
  (let ((proc-dir (list-directory #P"/proc/acpi/thermal_zone/"))
        (sys-dir (sort
                  (remove-if-not
                   (lambda (x)
                     (when (and (cl-ppcre:scan "^.*/thermal_zone\\d+/" (namestring x))
                                (string-equal (alexandria:read-file-into-string (format nil "~A/type" x)) (format nil "x86_pkg_temp~%")))
                       x))
                   (list-directory #P"/sys/class/thermal/"))
                  #'string< :key #'namestring)))
    (cond
      (proc-dir
       (cons :procfs
             (make-pathname :directory (pathname-directory (first proc-dir))
                            :name "temperature")))
      (sys-dir
       (cons :sysfs
             (make-pathname :directory (pathname-directory (first sys-dir))
                            :name "temp"))))))

(defun fmt-cpu-temp ()
  "Returns a string representing the current CPU temperature."
  (let ((tempval (case (car *acpi-thermal-zone*)
                   (:procfs (parse-integer
                             (get-proc-file-field (cdr *acpi-thermal-zone*) "temperature")
                             :junk-allowed t))
                   (:sysfs   (with-open-file (f (cdr *acpi-thermal-zone*))
                               (/ (read f) 1000))))))
    (format nil "^[~A~,1F°C^]"
            (bar-zone-color tempval *cpu-temp-med* *cpu-temp-hi* *cpu-temp-crit*)
            tempval)))

(defun cpu-modeline (ml)
  (declare (ignore ml))
  (format-expand *cpu-formatters-alist*
                 *cpu-modeline-fmt*))

(defvar *cpu-formatters-alist*
  '((#\c  fmt-cpu-usage)
    (#\C  fmt-cpu-usage-bar)
    (#\f  fmt-cpu-freq)
    (#\r  fmt-cpu-freq-range)
    (#\t  fmt-cpu-temp)))

(defvar *cpu-modeline-fmt* "%c (%f) %t"
  "The default value for displaying cpu information on the modeline.

@table @asis
@item %%
A literal '%'
@item %c
CPU usage
@item %C
CPU usage graph
@item %f
CPU frequency
@item %r
CPU frequency range
@item %t
CPU temperature
@end table
")
