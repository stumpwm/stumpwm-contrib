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
(add-screen-mode-line-formatter #\M 'fmt-mem-usage)
(add-screen-mode-line-formatter #\N 'fmt-mem-usage-bar)

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

(defun fmt-mem-usage (ml)
  "Returns a string representing the current percent of used memory."
  (declare (ignore ml))
  (let* ((mem (mem-usage))
	 (|%| (truncate (* 100 (nth 2 mem))))
	 (allocated (truncate (/ (nth 1 mem) 1000))))
    (format nil "MEM: ~4D mb ^[~A~3D%^] " allocated (bar-zone-color |%|) |%|)))

(defun fmt-mem-usage-bar (ml &optional (width *mem-usage-bar-width*) (full *mem-usage-bar-full*) (empty *mem-usage-bar-empty*))
  "Returns a coloured bar-graph representing the current allocation of memory."
  (declare (ignore ml))
  (let ((cpu (truncate (* 100 (nth 2 (mem-usage))))))
    (stumpwm::bar cpu width full empty)))
