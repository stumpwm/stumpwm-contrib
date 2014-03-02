;;;; undocumented.lisp

(in-package #:undocumented)

;;; "undocumented" goes here. Hacks and glory await!

;; Copyright (C) 2011 Ben Spencer
;;

;; Code:

(defun list-undocumented (&optional (manual #p"stumpwm.texi.in"))
  "List symbols that are exported from the stumpwm package and have
  documentation strings but do not appear in the manual"
  (let ((documented '()))
    (with-open-file (s manual :direction :input)
      (loop for line = (read-line s nil s)
         until (eq line s) do
           (ppcre:register-groups-bind (sym) ("^[@%#\\$!]{3} (.*)" line)
             (push sym documented))))
    (loop for sym being the external-symbols in :stumpwm
       when (and (or (documentation sym 'function)
                     (documentation sym 'variable))
                 (not (find sym documented :test #'string-equal)))
       collecting sym)))
