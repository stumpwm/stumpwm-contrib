;;;; surfraw.lisp

(in-package #:surfraw)

;;; "surfraw" goes here. Hacks and glory await!

;; SURFRAW module for StumpWM.
;;
;; Copyright (C) 2008 Ivy Foster
;; Copyright (C) 2010 Ivy Foster, Friedrich Delgado
;;
;; Maintainer: Ivy Foster
;;

;;; Code:

(defun split-by-- (str)
  (let ((pos (position #\- str :start (1+ (position #\- str)))))
    (list (subseq str 0 (1- pos))
          (subseq str (1+ pos)))))

(defun surfraw-elvis-list ()
  (mapcar (lambda (x)
            (mapcar (lambda (x) (string-trim '(#\Space #\Tab #\Newline) x))
                    (split-by-- x)))
          (remove-if-not #'(lambda (string) (search "--" string))
                         (split-string (run-shell-command "surfraw -elvi" :collect-output-p)
                                       '(#\Newline)))))

(auto-define-surfraw-commands-from-elvis-list)
;;; Regular surfraw commands

(defcommand surfraw (engine search)
  ((:string "What engine? ") (:string "Search for what? "))
  "Use SURFRAW to surf the net; reclaim heathen lands."
  (check-type engine string)
  (check-type search string)
  (run-shell-command (concat "exec surfraw -g " engine " " search)))

;;; Bookmarks

(defun display-file (file)
  "Display a file in the message area."
  (if (probe-file file)
      (run-shell-command (concat "cat " file) t)
    (message "The file ~a does not exist." file)))

(defvar *surfraw-bookmark-file* nil
  "The surfraw bookmark file")

(defcommand sr-bookmark (bmk) ((:string "Bookmark: "))
  (surfraw "" bmk))

(defcommand sr-bookmark-file-display () ()
  (display-file *surfraw-bookmark-file*))

;;; surfraw.lisp ends here
