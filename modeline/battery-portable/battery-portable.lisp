;;;; battery-portable.lisp

(in-package #:battery-portable)

;;; "battery-portable" goes here. Hacks and glory await!

;;; Portable battery information for StumpWM's mode-line.
;;;
;;; Written by Julian Stecklina with inspiration from John Li and
;;; Rupert Swarbrick.
;;;
;;; Copyright (c) 2008 Julian Stecklina
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;

;;; CLISP doesn't include :linux in *features* even if it runs on
;;; Linux. :-/

;;; Configuration

(defvar *refresh-time* 5
  "Time in seconds between updates of battery information.")

(defvar *prefer-sysfs* t
  "This has no effect. Legacy purposes only.")

(defvar *preferred-drivers-failed* nil
  "T when the preferred battery method cannot find info.")

(defvar *non-preferred-drivers-failed* nil
  "T when the non-preferred battery method cannot find info.")

(defvar *no-battery-info* "(no battery info)"
  "Message to give when no battery info is found.")

;;; Method base class

(defclass battery-method ()
  ()
  (:documentation "Base class for battery information retrieval"))

(defgeneric all-batteries (method)
  (:documentation "Returns all recognized batteries."))

(defun preferred-battery-method (&optional (sysfs t))
  #- (or linux openbsd)
  nil
  #+ linux
  (if sysfs
      (make-instance 'sysfs-method)
      (make-instance 'procfs-method))
  #+ openbsd
  (make-instance 'usr-sbin-apm-method))

;;; Battery class

(defclass battery ()
  ()
  (:documentation "Base class for battery information."))

(defgeneric state-of (battery)
  (:documentation "Returns either :UNKNOWN, :CHARGED, :CHARGING,
  or :DISCHARGING with the obvious meanings. If the state is
  not :UNKNOWN, returns the battery fill percentage. If the state
  is :CHARGING or :DISCHARGING, this function returns a third value
  indicating the corresponding time in seconds."))

;;; Linux procfs implementation

#+ linux
(progn
  (defclass procfs-method (battery-method)
    ()
    (:documentation "Collect battery information through Linux' procfs interface."))

  (defclass procfs-battery (battery)
    ((path :initarg :path :initform (error ":path missing")
           :reader path-of)
     (info-hash :initform (make-hash-table :test 'equal)
                :reader info-hash-of)))

  (defmethod update-info ((battery procfs-battery))
    (clrhash (info-hash-of battery))
    (loop
       for filename in '("state" "info")
       do (with-open-file (file (merge-pathnames (make-pathname :name filename)
                                                 (path-of battery)))
            (loop
               for line = (read-line file nil nil)
               while line
               do (multiple-value-bind (match? matches)
                      (scan-to-strings "^([^:]+):\\s*([^\\s]+)(\\s.*)?$" line)
                    (if (not match?)
                        (format t "Unrecognized line: ~S~%" line)
                        (setf (gethash (aref matches 0) (info-hash-of battery))
                              (aref matches 1))))))))

  (define-condition info-value-not-present (error)
    ())

  (defmethod info-value ((battery procfs-battery) key)
    (multiple-value-bind (val found?)
        (gethash key (info-hash-of battery))
        (if found?
            val
            (error 'info-value-not-present))))

  (defmethod info-value-int ((battery procfs-battery) key)
    (values (parse-integer (info-value battery key))))

  (defmethod all-batteries ((method procfs-method))
    (mapcar (lambda (p)
              (make-instance 'procfs-battery :path p))
            (list-directory "/proc/acpi/battery/")))

  (defmethod state-of ((battery procfs-battery))
    (handler-case
        (progn
          (update-info battery)
          (if (string/= (info-value battery "present") "yes")
              :unknown
              (let* ((state (info-value battery "charging state")))
                (flet ((percent ()
                         (/ (info-value-int battery "remaining capacity")
                            (info-value-int battery "last full capacity"))))

                (cond
                  ((string= state "charged") (values :charged (percent)))
                  ((string= state "discharging")
                   (values :discharging (percent)
                           (* 3600 (/ (info-value-int battery "remaining capacity")
                                      (info-value-int battery "present rate")))))
                  ((string= state "charging")
                   (values :charging (percent)
                           (* 3600 (/ (- (info-value-int battery "last full capacity")
                                         (info-value-int battery "remaining capacity"))
                                      (info-value-int battery "present rate")))))
                  (t :unknown))))))
      (t () :unknown))))

;;; Linux sysfs implementation

#+ linux
(progn

  (defclass sysfs-method (battery-method)
    ()
    (:documentation "Collect battery information through Linux'
  class-based sysfs interface."))

  (defclass sysfs-battery (battery)
    ((path :initarg :path :initform (error ":path missing")
           :reader path-of)))

  (defun sysfs-field (path name)
    (with-open-file (file (merge-pathnames (make-pathname :name name)
                                           path))
      (read-line-from-sysfs file)))

  (defun sysfs-int-field (path name)
    (handler-case (parse-integer (sysfs-field path name) :junk-allowed t)
      (file-error () nil)
      (simple-error () nil)))

  (defmethod all-batteries ((m sysfs-method))
    (remove nil
            (mapcar (lambda (path)
                      (handler-case
                          (when (string= "Battery"
                                         (sysfs-field path "type"))
                            (make-instance 'sysfs-battery
                                           :path path))
                        (file-error () nil)))
                    (list-directory "/sys/class/power_supply/"))))

  (defmethod state-of ((battery sysfs-battery))
    (handler-case
        (let* ((path (path-of battery))
               (present (sysfs-field path "present")))
          (if (or (not present) (string= present "0"))
              :unknown
              (labels ((full ()
                         (or (sysfs-int-field path "energy_full")
                             (sysfs-int-field path "charge_full")))
                       (curr ()
                         (or (sysfs-int-field path "energy_now")
                             ;; energy_* seems not to be there on
                             ;; some boxes. Strange...
                             (sysfs-int-field path "charge_now")))
                       (consumption ()
                         (or (sysfs-int-field path "power_now")
                             (sysfs-int-field path "current_now")))
                       (capacity-native ()
                         (sysfs-int-field path "capacity"))
                       (capacity-calculate ()
                         (let ((curr (curr))
                               (full (full)))
                           (and curr full
                                (* 100 (/ curr full)))))
                       (capacity-level ()
                         (sysfs-field path "capacity_level")))
                (let ((capacity (or (capacity-native)  ;Try better options first.
                                    (capacity-calculate)
                                    (capacity-level))))
                  (if (null capacity)
                      :unknown
                      (let* ((state (sysfs-field path "status"))
                             (state (or (and (stringp state)
                                             (cond ((string= state "Charging") :charging)
                                                   ((string= state "Discharging") :discharging)
                                                   ((or (string= state "Full")
                                                        (string= state "Not charging"))
                                                    :charged)
                                                   (t :unknown)))
                                        :unknown)))
                        (values state
                                capacity
                                (let ((full (full))
                                      (curr (curr))
                                      (consumption (consumption)))
                                  (if (or (null consumption)
                                          (null full)
                                          (null curr)
                                          (zerop consumption))
                                      0
                                      (cond
                                        ((eql state :charged) nil)
                                        ((eql state :discharging)
                                         (* 3600 (/ curr consumption)))
                                        ((eql state :charging)
                                         (* 3600 (/ (- full curr) consumption)))))))))))))
      (t () :unknown))))

;;; OpenBSD /usr/sbin/apm implementation

#+ openbsd
(progn
  (defclass usr-sbin-apm-method (battery-method) ()
    (:documentation "Collect battery information through OpenBSD' /usr/sbin/apm program."))

  (defclass usr-sbin-apm-battery (battery) ())

  (defun read-usr-sbin-apm-info ()
    (with-input-from-string (apm (run-shell-command "/usr/sbin/apm -ablm" t))
      (let* ((state (ignore-errors (parse-integer (read-line apm))))
             (percent (ignore-errors (parse-integer (read-line apm))))
             (minutes (ignore-errors (parse-integer (read-line apm))))
             (ac (ignore-errors (parse-integer (read-line apm)))))
        (unless (and (or (null state) (eql state 4))
                     (or (null ac) (eql ac 255)))
          (values (case state
                    (0 :high)
                    (1 :low)
                    (2 :critical)
                    (3 :charging)
                    (4 :absent)
                    (t :unknown))
                  percent
                  minutes
                  (case ac
                    (0 :disconnected)
                    (1 :connected)
                    (2 :backup)
                    (t :unknown)))))))

  (defmethod all-batteries ((method usr-sbin-apm-method))
    (unless (null (read-usr-sbin-apm-info))
      (list (make-instance 'usr-sbin-apm-battery))))

  (defmethod state-of ((battery usr-sbin-apm-battery))
    (multiple-value-bind (state percent minutes ac)
        (read-usr-sbin-apm-info)
      (let ((percent (or percent 0))
            (seconds (when minutes (* minutes 60))))
        (case ac
          ((:disconnected :backup)
           (values :discharging percent seconds))
          (:connected
           (cond
             ((or (eql state :absent)
                  (eql state :unknown))
              (values :unknown))
             ((eql percent 100)
              (values :charged percent))
             (t
              (values :charging percent seconds))))
          (t
           (values :unknown)))))))

;;; Interface to the outside world.

(defun fmt-time (stream arg colonp atp)
  (declare (ignore colonp atp))
  (when (and (numberp arg)
             (plusp arg))
    (multiple-value-bind (hours rest)
        (truncate arg 3600)
      (format stream "~D:~2,'0D" hours (floor rest 60)))))

(defun battery-info-string ()
  "Compiles a string suitable for StumpWM's mode-line."
  (with-output-to-string (fmt)
    (let ((current-fs (cond ((and *preferred-drivers-failed*
                                  *non-preferred-drivers-failed*)
                             (return-from battery-info-string
                               "(not implemented)"))
                            ((and *preferred-drivers-failed*
                                  (not *non-preferred-drivers-failed*))
                             nil)
                            (t t))))
      (let ((batteries (all-batteries (preferred-battery-method current-fs))))
       (if (endp batteries)
           (progn (format fmt "~A" *no-battery-info*)
                  (if current-fs
                      (setf *preferred-drivers-failed* t)
                      (setf *non-preferred-drivers-failed* t)))
           (loop
             for bat in batteries
             do (multiple-value-bind (state perc time)
                    (state-of bat)
                  (ecase state
                    (:unknown (format fmt "~A" *no-battery-info*))
                    (:charged (etypecase perc
                                (string (format fmt "~~ ~A" perc))
                                (number (format fmt "~~ ~D%" (round perc)))))
                    ((:charging :discharging)
                     (etypecase perc
                       (string (format fmt "~/battery-portable::fmt-time/~A ^[~A~A^]"
                                       time
                                       (if (eq state :charging) #\+ #\-)
                                       (bar-zone-color (cond ((or (string= "Low" perc)
                                                                  (string= "Critical" perc)
                                                                  (string= "Unknown" perc)) 19)
                                                             ((string= "Normal" perc) 50)
                                                             ((or (string= "High" perc)
                                                                  (string= "Full" perc)) 91))
                                                       90 50 20 t)
                                       perc))
                       (number (format fmt "~/battery-portable::fmt-time/~A ^[~A~A%^]"
                                       time
                                       (if (eq state :charging) #\+ #\-)
                                       (bar-zone-color perc 90 50 20 t)
                                       (round perc)))))))))))))

;;; The actual mode-line format function. A bit ugly...
(let ((next 0)
      (last-value ""))
  (defun fmt-bat (ml)
    (declare (ignore ml))
    ;; Return the last info again, if we are called too quickly.
    (let ((now (get-universal-time)))
      (when (< now next)
        (return-from fmt-bat last-value))
      (setf next (+ now *refresh-time*)))
    ;; Generate info string.
    (setf last-value (battery-info-string))))

;;; Put this at the end to avoid evaluating it when the core above
;;; throws an error.

(add-screen-mode-line-formatter #\B #'fmt-bat)

;;; EOF
