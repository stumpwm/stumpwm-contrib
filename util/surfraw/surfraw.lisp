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

(defun surfraw-elvis-list ()
  (macrolet ((trim (str) `(string-trim '(#\Space #\Tab) ,str)))
    (loop :for line :in (run-program '("surfraw" "-elvi") :output :lines)
          :for pos = (search "--" line)
          :when pos
            :collect (list (trim (subseq line 0 pos))
                           (trim (subseq line (+ pos 2)))))))

;;; Regular surfraw commands

(define-stumpwm-type :surfraw-elvi (input prompt)
  (let ((elvis (mapcar 'car (surfraw-elvis-list))))
    (or (find (or (argument-pop input)
                  (completing-read (current-screen) prompt elvis :require-match t)
                  (throw 'error "Abort"))
              elvis :test 'string=)
        (throw 'error "Such elvi doesn't exist"))))

(defcommand surfraw (engine search)
    ((:surfraw-elvi "What engine? ")
     (:string "Search for what? "))
  "Use SURFRAW to surf the net; reclaim heathen lands."
  (check-type engine string)
  (check-type search string)
  (run-shell-command (concat "exec surfraw -g " engine " " search)))

;;; Bookmarks

(defvar *surfraw-bookmark-file* nil
  "The surfraw bookmark file")

(defcommand sr-bookmark (bmk) ((:string "Bookmark: "))
  (run-shell-command (concat "exec surfraw -g " bmk)))

(defcommand sr-bookmark-file-display () ()
  (if-let ((path (file-exists-p *surfraw-bookmark-file*)))
    (read-file-string path)
    (message "The file ~a does not exist." file)))

;;; surfraw.lisp ends here
