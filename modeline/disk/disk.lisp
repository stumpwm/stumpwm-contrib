;;;; disk.lisp

(in-package :disk)

;;; "disk" goes here. Hacks and glory await!

;;; Disk usage monitoring for stumpwm's modeline
;;;
;;; Copyright 2007 Morgan Veyret.
;;;
;;; Maintainer: Morgan Veyret
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; CODE:

(add-screen-mode-line-formatter #\D 'disk-modeline)

(defparameter *disk-usage* nil)

(defparameter *disk-formatters-alist*
  '((#\d  disk-get-device)
    (#\s  disk-get-size)
    (#\u  disk-get-used)
    (#\a  disk-get-available)
    (#\p  disk-get-use-percent)
    (#\m  disk-get-mount-point)))

(defparameter *disk-modeline-fmt* "%m: %u/%s"
  "The default value for displaying disk usage information on the modeline.

@table @asis
@item %%
A literal '%'
@item %d
Filesystem device
@item %s
Filesystem size
@item %u
Filesystem used space
@item %a
Filesystem available space
@item %p
Filesystem used space in percent
@item %m
Filesystem mount point
@end table
")

(defparameter *disk-usage-paths* '("/")
  "The list of mount points to report the disk usage of.")

(defun disk-usage-tokenize (usage-line-str)
  (ppcre:split "(\\s+)" usage-line-str))

(defun disk-update-usage (paths)
  (setf *disk-usage*
        (with-input-from-string
            (usage-str (run-shell-command
                        (format nil "df -h ~{~a ~} | grep -v 'File system'" paths) t))
          (loop for i = (read-line usage-str nil nil)
             while i
             collect (disk-usage-tokenize i)))))

(defun disk-usage-get-field (path field-number)
  (let ((usage-infos (find-if (lambda (item)
                                (string= (car (last item)) path))
                              *disk-usage*)))
    (nth field-number usage-infos)))

(defun size-human-readable (size-as-number)
  (diskspace:size-in-human-readable size-as-number))

(defun disk-get-size-as-number (path)
  (diskspace:disk-total-space path))

(defun disk-get-size (path)
  (diskspace:disk-total-space path t))

(defun disk-get-used-as-number (path)
   (- (disk-get-size-as-number path)
      (disk-get-available-size-as-number path)))

(defun disk-get-used (path)
  (size-human-readable (disk-get-used-as-number path)))

(defun disk-get-available-size-as-number (path)
  (diskspace:disk-available-space path))

(defun disk-get-available (path)
  (diskspace:disk-available-space path t))

(defun disk-get-use-percent (path)
  (let ((value (truncate (* 100
                            (/ (disk-get-used-as-number path)
                               (disk-get-size-as-number path))))))

    (format nil "~a%" value)))

;;; whe should use fallback mode (i.e. shell + df) for this two :(
;;; any idea how to improve this?

(defun disk-get-device (path)
  (disk-usage-get-field path 0))

(defun disk-get-mount-point (path)
  (disk-usage-get-field path 5))

(defun use-fallback-method-p ()
  (or (search "%m" *disk-modeline-fmt* :test #'string=)
      (search "%d" *disk-modeline-fmt* :test #'string=)))

(defun disk-modeline (ml)
  (declare (ignore ml))
  (when (use-fallback-method-p)
    (disk-update-usage *disk-usage-paths*))
  (let ((fmts (loop for p in *disk-usage-paths* collect
                   (format-expand *disk-formatters-alist*
                                  *disk-modeline-fmt*
                                  p))))
    (format nil "~{~a ~}" fmts)))
