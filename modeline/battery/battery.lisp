;;; Copyright 2008 Vitaly Mayatskikh
;;; Copyright 2015 Javier Olaechea
;;;

(in-package #:cl-user)

(defpackage #:battery
  (:use #:cl #:stumpwm)
  (:export #:*battery-name
           #:*battery-charge-refresh-time*)
  (:documentation "Battery charge formatters for the mode-line."))

(in-package #:battery)

(dolist (a '((#\b fmt-bat-charge)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

(defvar *bat-state* nil "Is the battery charging or discharging?")

(defvar *bat-remain* 0)

(defvar *bat-remain-time* nil)

(defvar *bat-prev-time* 0)

(defvar *battery-charge-refresh-time* 15
  "The minimum interval of time, in seconds, that has to pass before
recalculating the battery charge.")

(defvar *battery-name* "BAT0"
  "The name of the battery")
(defvar *battery-path* "/proc/acpi/battery/"
  "The path where to look for batteries.")

(defun list-subdirectories (directory)
  "List the name, relative to the `directory' of the sub-directories contained
in the `directory'"
  (mapcar (lambda (subdir)
            (multiple-value-bind
                  (abs-or-rel file-parts)
                (uiop:split-unix-namestring-directory-components
                 (uiop:native-namestring subdir))
              (declare (ignore abs-or-rel))
              (car (last file-parts))))
          (uiop:subdirectories directory)))

(define-condition battery-not-found (stumpwm-error file-error)
  ((battery-name :initarg :battery-name :reader battery-name
                 :documentation "The battery-name that was searched for."))
  (:report (lambda (condition stream)
             (format stream
                     "The Battery ~A was not found. Valid Battery names are:~{ ~A~^,~}."
                     (battery-name condition)
                     (list-subdirectories *battery-path*))))
  (:documentation "Signaled when the battery was not found in
  `*battery-path*'"))

(defun %read-battery-file (battery)
  "Reads the content of the battery file. To be called by `read-battery-file'"
  (let ((fields (make-hash-table :test #'equal)))
    (loop
      :for line := (read-line battery nil 'eof)
      :until (eql line 'eof)
      :for split-line := (cl-ppcre:split ":\\s*" line)
      :do
         (setf (gethash (string-trim '(#\Space) (car split-line)) fields)
               (string-trim '(#\Space) (cadr split-line))))
    fields))

(defun read-battery-file (battery fname)
  (handler-bind
      ;; Resignal file-error as a battery-not-found error.
      ((file-error (lambda (c)
                     (declare (ignore c))
                     (error 'battery-not-found :battery-name battery))))
    (restart-case
        (with-open-file (battery-file (concatenate 'string *battery-path* battery "/" fname))
          (%read-battery-file battery-file))
      (chose-new-battery-name (new-battery)
        :report (lambda (stream)
                  (format stream "The battery ~A was not found. Choose a new battery name:" battery))
        :interactive (lambda ()
                       (format *query-io* "~@<Enter the new battery name: ~@:>")
                       (finish-output *query-io*)
                       (list (read *query-io*)))
        (read-battery-file new-battery fname)))))

(defun current-battery-charge ()
  "Calculate remaining battery charge."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (or (= 0 *bat-prev-time*) (>= (- now *bat-prev-time*) *battery-charge-refresh-time*))
      (setf *bat-prev-time* now)
      (let ((battery-state (read-battery-file *battery-name* "state"))
            (battery-info (read-battery-file *battery-name* "info")))
        (if (string= "no" (gethash "present" battery-state))
            (setf *bat-state* nil)
            (let ((charge-state (gethash "charging state" battery-state))
                  (remain (parse-integer (gethash "remaining capacity" battery-state)
                                         :junk-allowed t))
                  (rate (/ (or (parse-integer (gethash "present rate" battery-state)
                                              :junk-allowed t) 0) 60))
                  (full (parse-integer (gethash "last full capacity" battery-info)
                                       :junk-allowed t)))
              (setf *bat-remain* (round (/ (* 100 remain) full))
                    *bat-state* charge-state
                    *bat-remain-time* nil)

              (when (> rate 0)
                (let* ((online (round (/ (if (string= "charging" *bat-state*)
                                             (- full remain) remain)
                                         rate))))
                  (setf *bat-remain-time* (multiple-value-bind (h m)
                                              (truncate online 60)
                                            (list h m)))))))))))

(defun fmt-bat-charge (ml)
  "Returns a string representing the remaining battery charge (for laptop users.)"
  (declare (ignore ml))
  (current-battery-charge)
  (if *bat-state*
      (format nil "BAT: ^[~A~D%^]~A"
              (bar-zone-color *bat-remain* 50 30 10 t)
              *bat-remain*
              (if *bat-remain-time*
                  (format nil " (~2,'0d:~2,'0d) ~A"  (car *bat-remain-time*) (cadr *bat-remain-time*) *bat-state*) "")) "no battery"))


;; Alternative display:
;;
;;    TT: RRR% (HH:MM)  [or "NO BAT" if present = no]
;;
;;   TT    = AC/DC (AC if charging state = charged/charging,
;;                  DC if charging state = discharging)
;;
;;   RRR   = remain/full
;;
;;   HH:MM = time until charged/discharged (present when state is charging
;;                                          or discharging)
;;
;; (defun fmt-bat-charge (ml)
;;   "Returns a string representing the remaining battery charge (for laptop users.)"
;;   (declare (ignore ml))
;;   (current-battery-charge)
;;   (if (not *bat-state*)
;;       "NO BAT"
;;       (format nil "~A:~D%~A"
;;               (if (or (string= *bat-state* "charging")
;;                       (string= *bat-state* "charged"))
;;                   "AC" "DC")
;;            *bat-remain*
;;            (if (and (string/= *bat-state* "charged") *bat-remain-time*)
;;                (format nil (if (and (= (car *bat-remain-time*) 0)
;;                                        (< (cadr *bat-remain-time*) 30))
;;                                   " (^[^B^1*~2,'0d:~2,'0d^])" " (~2,'0d:~2,'0d)")
;;                           (car *bat-remain-time*)
;;                           (cadr *bat-remain-time*))
;;                   ""))))
