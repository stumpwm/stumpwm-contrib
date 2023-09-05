;;;; pianobar.lisp

(in-package #:pianobar)

;;; "pianobar" goes here. Hacks and glory await!

;;; Pianobar now playing info
;;;
;;; Copyright 2023 Ahmed Khanzada
;;;
;;; Maintainer:
;;;

;; Install formatters.

(add-screen-mode-line-formatter #\P 'pianobar-modeline)

;; Usage

(defun pianobar-read-file-as-string (file-path)
  (with-open-file (stream file-path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun pianobar-now-playing ()
  (let ((file-path "~/.config/pianobar/nowplaying"))
    (pianobar-read-file-as-string file-path)))

(defun fmt-pianobar-now-playing (now-playing)
  "Returns Pianobar now playing info"
  now-playing)

(defun pianobar-modeline (ml)
  (declare (ignore ml))
  (format-expand *pianobar-formatters-alist*
                 *pianobar-modeline-fmt*
                 (pianobar-now-playing)))

(defvar *pianobar-formatters-alist*
  '((#\p  fmt-pianobar-now-playing)))

(defvar *pianobar-modeline-fmt* "%p"
  "The default value for displaying pianobar usage information on the modeline.

@table @asis
@item %%
A literal '%'
@item %p
Now playing
")
