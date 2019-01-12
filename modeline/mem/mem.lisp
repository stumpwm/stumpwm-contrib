;;;; mem.lisp

(in-package #:mem)

;;; "mem" goes here. Hacks and glory await!

;;; MEM formatters for the mode-line
;;;
;;; Copyright 2009 Vitaly Mayatskikh
;;;
;;; Maintainer:
;;;

;; Install formatters.
(add-screen-mode-line-formatter #\M 'mem-modeline)

;; Defaults arguments for fmt-mem-usage-bar
(defvar *mem-usage-bar-width* 10)
(defvar *mem-usage-bar-full* #\#)
(defvar *mem-usage-bar-empty* #\:)

(defun get-proc-fd-field (s field)
  (if s
      (do ((line (read-line s nil nil) (read-line s nil nil)))
	  ((null line) nil)
	(let ((split (cl-ppcre:split "\\s*:\\s*" line)))
	  (when (string= (car split) field) (return (cadr split)))))
      ""))

(defun mem-usage ()
  "Returns a list containing 3 values:
total amount of memory, allocated memory, allocated/total ratio"
  (let ((allocated 0))
    (multiple-value-bind (mem-total mem-free buffers cached)
	(with-open-file (file #P"/proc/meminfo" :if-does-not-exist nil)
	  (values
	   (read-from-string (get-proc-fd-field file "MemTotal"))
	   (read-from-string (get-proc-fd-field file "MemFree"))
	   (read-from-string (get-proc-fd-field file "Buffers"))
	   (read-from-string (get-proc-fd-field file "Cached"))))
      (setq allocated (- mem-total (+ mem-free buffers cached)))
      (list mem-total allocated (/ allocated mem-total)))))

(defun fmt-mem-allocated (mem)
  "Returns a string representing the current allocated memory."
  (let* ((allocated (truncate (/ (nth 1 mem) 1000))))
    (format nil "~4D mb" allocated)))

(defun fmt-mem-percent (mem)
  "Returns a string representing the current percent of used memory."
  (let* ((% (truncate (* 100 (nth 2 mem)))))
    (format nil "^[~A~3D%^] " (bar-zone-color %) %)))

(defun fmt-mem-usage-bar (mem)
  "Returns a coloured bar-graph representing the current allocation of memory."
  (let ((cpu (truncate (* 100 (nth 2 mem)))))
    (stumpwm:bar cpu *mem-usage-bar-width* *mem-usage-bar-full* *mem-usage-bar-empty*)))

(defun mem-modeline (ml)
  (declare (ignore ml))
  (format-expand *mem-formatters-alist*
                 *mem-modeline-fmt*
                 (mem-usage)))

(defvar *mem-formatters-alist*
  '((#\a  fmt-mem-allocated)
    (#\p  fmt-mem-percent)
    (#\b  fmt-mem-usage-bar)))

(defvar *mem-modeline-fmt* "MEM: %a %p"
  "The default value for displaying mem usage information on the modeline.

@table @asis
@item %%
A literal '%'
@item %a
Allocated memory in MB
@item %p
Percent of used memory
@item %b
Bar graph of current memory allocation
@end table
")

