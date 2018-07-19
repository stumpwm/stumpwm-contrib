;;;; utils.lisp

(in-package #:desktop-entry)
(defun string-split (regex target-string)
  (cl-ppcre:split regex target-string))

(defun string-replace-all (regex target-string replacement)
  (cl-ppcre:regex-replace-all regex target-string replacement))

(defun list-directory (path)
  (stumpwm:list-directory path))

(defun list-entry-files (path)
  (remove-if-not
   #'(lambda (file) (search "desktop" (file-namestring file)))
   (list-directory path)))
