;;;; mpd.lisp

(in-package #:mpd)
;;; MPD client & formatters for stumpwm
;;;
;;; Copyright 2007-2008 Morgan Veyret, Ivy Foster.
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

;;; Various changes in this file by Tom Hunt, 2012-2015.

;;; CODE:

#-(or sbcl clisp lispworks6 ccl)
(error "Unimplemented for ~S." (lisp-implementation-type))

(export '(*mpd-timeout*
	  *mpd-collapse-album-length*
	  *mpd-collapse-all-length*
	  *mpd-current-song-fmt*
	  *mpd-formatters-alist*
	  *mpd-status-fmt*
	  *mpd-modeline-fmt*
	  *mpd-volume-step*
	  *mpd-port*
	  *mpd-server*
	  mpd-browse-playlist
	  select-song-from-playlist
	  mpd-play
	  mpd-toggle-repeat
	  mpd-toggle-random
	  mpd-toggle-single
	  mpd-toggle-pause
	  mpd-kill
	  mpd-disconnect
	  mpd-connect
          *mpd-browse-menu-map*
	  *mpd-playlist-menu-map*
	  mpd-browse-artists
	  mpd-browse-albums
          mpd-browse-genres
          mpd-browse-tracks
          mpd-browse-database
	  mpd-update
	  mpd-clear
	  mpd-volume-down
	  mpd-volume-up
	  mpd-set-volume
	  mpd-prev
	  mpd-next
	  mpd-stop
	  mpd-play-track
          mpd-remove-track
          mpd-swap-tracks
	  mpd-search-genre
	  mpd-search-album
	  mpd-search-title
	  mpd-search-file
	  mpd-search-artist
	  mpd-search-and-add-genre
	  mpd-search-and-add-album
	  mpd-search-and-add-title
	  mpd-search-and-add-file
	  mpd-search-and-add-artist
	  mpd-add-file
	  mpd-playlist
	  mpd-status
	  mpd-current-song
	  *mpd-map*
	  *mpd-add-map*
	  *mpd-browse-map*
	  *mpd-search-map*))

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

;;mpd client
(defparameter *mpd-socket* nil)
(defparameter *mpd-server*
  #+(or clisp lispworks ccl)
  "localhost"
  #+sbcl
  #(127 0 0 1)
  )
(defparameter *mpd-port* 6600)
(defparameter *mpd-password* nil)

(defvar *mpd-timeout* 50)

(defvar *mpd-timer* nil)

(defvar *mpd-collapse-album-length* nil)
(defvar *mpd-collapse-all-length* nil)

(defmacro with-mpd-connection (&body body)
  `(if *mpd-socket*
       (handler-case (progn ,@body)
         (error (c)
           (progn
             (message "Error with mpd connection: ~a" c)
             (setf *mpd-socket* nil)
             (when *mpd-timer*
               (cancel-timer *mpd-timer*)))))
     (message "Error: not connected to mpd")))

(defun mpd-send (command)
  "Send command to stream ending with newline"
  (with-mpd-connection
    (let ((cmd (concatenate 'string command (string #\Newline))))
      #+clisp
      (ext:write-char-sequence cmd *mpd-socket*)
      #+(or sbcl ccl)
      (write-sequence cmd *mpd-socket*)
      #+lispworks
      (write-sequence (ef:encode-lisp-string cmd :utf-8) *mpd-socket*))
    (force-output *mpd-socket*)))

(defun mpd-send-command (cmd)
  (mpd-send cmd)
  (mpd-receive))

(defun mpd-format-command (fmt &rest args)
  (mpd-send-command (apply 'format nil fmt args)))

(defun mpd-termination-p (str)
  (or (mpd-error-p str)
      (mpd-ok-p str)))

(defun mpd-error-p (str)
  (when (>= (length str) 3)
    (equal (subseq str 0 3) "ACK")))

(defun mpd-ok-p (str)
  (equal str "OK"))

(defun mpd-tokenize (str)
  (let ((pos (position #\: str)))
    (list (read-from-string (concatenate 'string
                                         ":"
                                         (subseq str 0 pos)))
          (subseq str (+ pos 2)))))

(defun assoc-value (name list)
  (cadr (assoc name list)))

(defun mpd-receive (&optional ignore-data-p)
  "Unless IGNORE-DATA-P is T returns a list containing all data sent by mpd."
  (with-mpd-connection
    (flet ((receive-line (stream)
             #+(or sbcl clisp ccl)
             (read-line stream)
             #+lispworks
             (let ((bytes
                     (loop for b = (read-byte stream)
                           until (char= (code-char b) #\Newline)
                           collect b)))
               (ef:decode-external-string (coerce bytes 'vector) :utf-8))))
      (if ignore-data-p
          (receive-line *mpd-socket*)
          (loop for i = (receive-line *mpd-socket*)
                when (mpd-error-p i)
                  do (message "Error sent back by mpd: ~a" i)
                until (mpd-termination-p i)
                collect (mpd-tokenize i))))))

(defun init-mpd-connection ()
  "Connect to mpd server"
  (handler-case
      (setf *mpd-socket*
            #+clisp
            (socket:socket-connect *mpd-port* *mpd-server* :element-type 'character)
            #+sbcl
            (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream :protocol :tcp)))
              (sb-bsd-sockets:socket-connect s *mpd-server* *mpd-port*)
              (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :none))
            #+lispworks
            (comm:open-tcp-stream *mpd-server* *mpd-port*
                                  :direction :io
                                  :element-type '(unsigned-byte 8)
                                  :errorp t)
            #+ccl
            (ccl:make-socket :remote-host *mpd-server*
                             :remote-port *mpd-port*
                             :external-format :utf-8))
    (error (err)
      (format t  "Error connecting to mpd: ~a~%" err)))
  (when *mpd-socket*
    (when *mpd-timeout*
      (setf *mpd-timer*
            (run-with-timer *mpd-timeout* *mpd-timeout* 'mpd-ping)))
    (mpd-receive t)
    (when *mpd-password*
      (mpd-format-command "password \"~a\"" *mpd-password*))))

(defun mpd-ping ()
  (mpd-send-command "ping"))

(defun mpd-search (type what &optional (exact-search nil))
  (mpd-format-command "~a ~a \"~a\""
                      (if exact-search "find"
                          "search")
                      type what))

(defun mpd-add (files)
  (loop for i in files
     do (mpd-format-command "add \"~a\"" i)))

;;; ------------------------------------------------------------------
;;; Formatting
;;; ------------------------------------------------------------------

(add-screen-mode-line-formatter #\m 'mpd-modeline)

(defparameter *mpd-current-song* nil)
(defparameter *mpd-status* nil)
(defparameter *mpd-did-repeat* nil)

(defun mpd-update-current-song ()
  (setf *mpd-current-song* (mpd-send-command "currentsong")))
(defun mpd-update-status ()
  (setf *mpd-status* (mpd-send-command "status")))

(defun mpd-get-artist ()
  (assoc-value :artist *mpd-current-song*))

(defun mpd-get-album ()
  (assoc-value :album *mpd-current-song*))

(defun mpd-get-date ()
  (assoc-value :date *mpd-current-song*))

(defun mpd-minutes-seconds (time)
  (let ((minutes) (seconds) (hours))
    (if (< time 60)
	(progn
	  (setf hours 0)
	  (setf minutes 0)
	  (setf seconds time))
      (multiple-value-bind (min sec) (floor time 60)
			   (setf minutes min)
			   (setf seconds sec)))
    (if (> minutes 59)
	(multiple-value-bind (hr min) (floor minutes 60)
			     (setf hours hr)
			     (setf minutes min))
      (setf hours 0))
    (when (< seconds 10)
      (setf seconds (concat "0" (write-to-string seconds))))
    (if (> hours 0)
	(format nil "~a:~2,,,'0@a:~a" hours minutes seconds)
      (format nil "~a:~a" minutes seconds))))

(defun mpd-get-elapsed ()
  (let* ((time (assoc-value :time *mpd-status*))
	 (pos (position #\: time))
	 (elapsed (parse-integer (subseq time 0 pos))))
    (mpd-minutes-seconds elapsed)))

(defun mpd-get-length ()
  (let ((time (parse-integer (or (assoc-value :time *mpd-current-song*) "0"))))
    (mpd-minutes-seconds time)))

(defun mpd-get-status ()
  (cond ((equal (assoc-value :state *mpd-status*) "play") "Playing")
        ((equal (assoc-value :state *mpd-status*) "pause") "Paused")
        ((equal (assoc-value :state *mpd-status*) "stop") "Stopped")))

(defun mpd-get-shortstatus () 
  (cond ((equal (assoc-value :state *mpd-status*) "play") "->")
        ((equal (assoc-value :state *mpd-status*) "pause") "||")
        ((equal (assoc-value :state *mpd-status*) "stop") "[]")))  

(defun mpd-get-file ()
  (assoc-value :file *mpd-current-song*))

(defun mpd-get-volume ()
  (assoc-value :volume *mpd-status*))

(defun mpd-get-xfade ()
  (let ((xfade (or (assoc-value :xfade *mpd-status*) "0")))
    (if (> (parse-integer xfade) 0)
        (format nil"F=~a" xfade) "_")))

(defun mpd-get-genre ()
  (assoc-value :genre *mpd-current-song*))

;; This function used to error out when called when nothing was playing, and
;; this fact caused me much strife and confusion. >:(
(defun mpd-get-number ()
  (write-to-string (1+ (parse-integer (or (assoc-value :song *mpd-status*) "-1")))))

(defun mpd-get-playlistlength ()
  (assoc-value :playlistlength *mpd-status*))

(defun mpd-repeating-p ()
  (if (string= (assoc-value :repeat *mpd-status*) "1")
      t nil))

(defun mpd-get-repeat ()
  (if (mpd-repeating-p) "R" "_"))

(defun mpd-shuffle-p ()
  (if (string= (assoc-value :random *mpd-status*) "1")
      t nil))

(defun mpd-get-shuffle ()
  (if (mpd-shuffle-p) "S" "_"))

(defun mpd-single-p ()
  (if (string= (assoc-value :single *mpd-status*) "1")
      t nil))

(defun mpd-get-single ()
  (if (mpd-single-p) "1" "_"))

(defun mpd-get-title ()
  (assoc-value :title *mpd-current-song*))

(defun mpd-get-label ()
  (let* ((title (assoc-value :title *mpd-current-song*))
         (filename (assoc-value :file *mpd-current-song*))
         (basename (subseq filename (1+ (or (position #\/ filename :from-end t) -1)) (position #\. filename :from-end t)))
         (realname (if (null title) basename title)))
    realname))

(defun mpd-get-track ()
  (assoc-value :track *mpd-current-song*))

(defun mpd-get-song-name ()
  (let* ((artist (assoc-value :artist *mpd-current-song*))
        (album (assoc-value :album *mpd-current-song*))
        (title (assoc-value :title *mpd-current-song*))
         (file (assoc-value :file *mpd-current-song*))
         (song (if (or (null artist)
                      (null album)
                      (null title))
                  (format nil "~a" file)
                   (format nil "~a \"~a\" - ~a"
                          artist
                          (if (and *mpd-collapse-album-length*
                                   (> (length album) *mpd-collapse-album-length*))
                              (concatenate 'string
                                           (subseq album 0 *mpd-collapse-album-length*)
                                           "...")
                              album)
                          title))))
    (if (and *mpd-collapse-all-length*
            (> (length song) *mpd-collapse-all-length*))
        (concatenate 'string (subseq song 0 *mpd-collapse-all-length*) "...")
       song)))

(defun mpd-modeline (ml)
  (declare (ignore ml))
  (if *mpd-socket*
      (with-mpd-connection
       (mpd-update-status)
       (if (equal "Stopped" (mpd-get-status))
           (format-expand *mpd-formatters-alist* *mpd-status-fmt*)
         (progn
           (mpd-update-current-song)
           (format-expand *mpd-formatters-alist* *mpd-modeline-fmt*))))
      "Not connected to mpd"))

(defvar *mpd-formatters-alist*
  '((#\a mpd-get-artist)
    (#\A mpd-get-album)
    (#\d mpd-get-date)
    (#\e mpd-get-elapsed)
    (#\f mpd-get-file)
    (#\F mpd-get-xfade)
    (#\g mpd-get-genre)
    (#\i mpd-get-single)
    (#\l mpd-get-length)
    (#\L mpd-get-shortstatus)
    (#\n mpd-get-number)
    (#\p mpd-get-playlistlength)
    (#\r mpd-get-repeat)
    (#\s mpd-get-shuffle)
    (#\S mpd-get-status)
    (#\t mpd-get-label)
    (#\T mpd-get-track)
    (#\v mpd-get-volume)
    (#\m mpd-get-song-name)))

(defvar *mpd-current-song-fmt* "%a
%A
%t
(%n/%p)"
  "The default value for displaying the current song.
For more information on valid formatters, please see the documentation
for `*mpd-modeline-fmt*'")

(defvar *mpd-status-fmt* "%S [%s;%r;%F]"
  "The default value for displaying the current MPD status.
For more information on valid formatters, please see the documentation
for `*mpd-modeline-fmt*'")

(defvar *mpd-modeline-fmt* "%S [%s;%r;%F]: %a - %A - %t (%n/%p)"
  "The default value for displaying MPD information on the modeline.

@table @asis
@item %%
A literal '%'
@item %a
Artist
@item %A
Album
@item %d
Date
@item %e
Elapsed time
@item %f
Filename
@item %F
'F=#' if crossfade is set, '_' otherwise
@item %g
Genre
@item %l
Song length
@item %n
Position in playlist (song Number, for a mnemonic)
@item %p
Total playlist length
@item %r
'R' if repeat is on, '_' otherwise
@item %s
'S' if shuffle is on, '_' otherwise
@item %S
'Playing' if playing, 'Paused' if paused, else 'Stopped'
@item %t
Title
@item %T
Track number (relative to the album, not the playlist)
@item %v
Volume
@end table
")

;;; ------------------------------------------------------------------
;;; Misc. commands
;;; ------------------------------------------------------------------

(defvar *mpd-volume-step* 5)

(defun mpd-menu (title options keymap &optional initial-selection)
  (multiple-value-bind (choice selection)
      (select-from-menu (current-screen) options title
                        (or initial-selection 0)
                        keymap)
    (cond
      ((null choice)
       (throw 'error "Abort."))
      (t (values choice selection)))))

(defun mpd-selected-item (menu)
  (nth (menu-state-selected menu) (menu-state-table menu)))

(defun mpd-menu-action (action-type)
  (lambda (menu)
    (throw :menu-quit
      (values action-type
              (mpd-selected-item menu)))))

;; playlist navigation/edition
(defvar *mpd-playlist-menu-map* nil)
(when (null *mpd-playlist-menu-map*)
  (setf *mpd-playlist-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "k") 'stumpwm::menu-up)
          (define-key m (kbd "j") 'stumpwm::menu-down)

          (define-key m (kbd "S-Up") (mpd-menu-action :mpd-playlist-move-up))
          (define-key m (kbd "S-Down") (mpd-menu-action :mpd-playlist-move-down))
	  (define-key m (kbd "S-k") (mpd-menu-action :mpd-playlist-move-up))
	  (define-key m (kbd "S-j") (mpd-menu-action :mpd-playlist-move-down))
          (define-key m (kbd "d") (mpd-menu-action :mpd-playlist-delete))
          (define-key m (kbd "RET") (mpd-menu-action :mpd-playlist-play))
          m)))

(defun mpd-uniq-and-sort-list (list criteria &optional do-sort case-insensitive)
  (let ((lst (mapcar #'cadr (remove-if (lambda (item)
                                         (not (equal criteria
                                                     (first item))))
                                       list))))
    (if do-sort
        (sort lst (if case-insensitive #'string-lessp #'string<))
      lst)))

(defcommand mpd-browse-playlist (&optional current-song) ()
  (let* ((status (mpd-send-command "status"))
         (response (mpd-send-command "playlistinfo"))
         (options (mpd-uniq-and-sort-list response :file)))
    (multiple-value-bind (action choice)
        (mpd-menu "Current playlist" options *mpd-playlist-menu-map*
                  (if current-song
                      current-song
                      (if (equal (assoc-value :state status) "play")
                          (parse-integer (assoc-value :song status))
                          0)))
      (let ((song-number (position choice options)))
        (case action
          (:mpd-playlist-move-up
           (if (= song-number 1)
               (mpd-browse-playlist song-number)
               (progn (mpd-swap-tracks song-number (1- song-number))
                      (mpd-browse-playlist (1- song-number)))))
          (:mpd-playlist-move-down
           (if (= song-number (length options))
             (mpd-browse-playlist song-number)
             (progn (mpd-swap-tracks song-number (1+ song-number))
                    (mpd-browse-playlist (1+ song-number)))))
          (:mpd-playlist-delete
           (when song-number
             (mpd-remove-track song-number)
             (mpd-browse-playlist song-number)))
          (:mpd-playlist-play
           (when song-number
             (mpd-play-track song-number))))))))

(defcommand-alias select-song-from-playlist browse-playlist)

(defvar *mpd-select-playlist-map* nil)
(when (null *mpd-select-playlist-map*)
  (setf *mpd-select-playlist-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "k") 'stumpwm::menu-up)
          (define-key m (kbd "j") 'stumpwm::menu-down)

          (define-key m (kbd "RET") (mpd-menu-action :mpd-playlist-load))
          m)))

(defcommand mpd-select-playlist (&optional current) ()
  (let* ((response (mpd-send-command "listplaylists"))
         (list (mpd-uniq-and-sort-list response :playlist t)))
    (multiple-value-bind (action choice)
        (mpd-menu "Playlists" list *mpd-select-playlist-map*
                  (if current current 0))
      (let ((index (position choice list)))
        (case action
          (:mpd-playlist-load
           (mpd-format-command "load ~a" choice)))))))

;; database  browsing
(defvar *mpd-browse-menu-map* nil)
(when (null *mpd-browse-menu-map*)
  (setf *mpd-browse-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "k") 'stumpwm::menu-up)
          (define-key m (kbd "j") 'stumpwm::menu-down)

          (define-key m (kbd "RET") (mpd-menu-action :mpd-browse-add-and-quit))
          (define-key m (kbd "S-RET") (mpd-menu-action :mpd-browse-add))

          (define-key m (kbd "Right") (mpd-menu-action :mpd-browse-next))
          (define-key m (kbd "Left") (mpd-menu-action :mpd-browse-previous))
          m)))


(defcommand mpd-browse-artists (&optional genre (startpos 0) waspos) ()
  (let* ((response (mpd-send-command
                    (if genre
                        (format nil "list artist genre \"~a\"" genre)
                          "list artist")))
         (options (mpd-uniq-and-sort-list response :artist t)))
        (multiple-value-bind (action choice)
            (mpd-menu "Select artist" options *mpd-browse-menu-map* startpos)
          (case action
            (:mpd-browse-add-and-quit
             (mpd-search-and-add-artist choice t))
            (:mpd-browse-add
             (mpd-search-and-add-artist choice t)
             (mpd-browse-artists genre (1+ (or (position choice options) 0)) waspos))
            (:mpd-browse-previous
             (unless %interactivep%
               (mpd-browse-genres (or (car waspos) 0) (cdr waspos))))
            (:mpd-browse-next
             (mpd-browse-albums choice genre 0 (cons (or (position choice options) 0) waspos)))))))

(defcommand mpd-browse-genres (&optional (startpos 0) waspos) ()
    (let* ((response (mpd-send-command "list genre"))
           (options (mpd-uniq-and-sort-list response :genre t)))
        (multiple-value-bind (action choice)
            (mpd-menu "Select genre" options *mpd-browse-menu-map* startpos)
          (case action
            (:mpd-browse-add-and-quit
             (mpd-search-and-add-genre choice t))
            (:mpd-browse-add
             (mpd-search-and-add-genre choice t)
             (mpd-browse-genres (1+ (or (position choice options) 0)) waspos))
            (:mpd-browse-next
               (mpd-browse-artists choice 0 (cons (or (position choice options) 0) waspos)))))))

(defcommand mpd-browse-albums (&optional artist genre (startpos 0) waspos) ((:string "Artist: "))
    (let* ((response (mpd-send-command
                      (if artist
                          (format nil "list album artist \"~a\"" artist)
                          "list album")))
           (options (mpd-uniq-and-sort-list response :album)))
        (multiple-value-bind (action choice)
            (mpd-menu "Select album" options *mpd-browse-menu-map* startpos)
          (case action
            (:mpd-browse-add-and-quit
             (mpd-search-and-add-album choice t))
            (:mpd-browse-add
             (mpd-search-and-add-album choice t)
             (mpd-browse-albums artist genre (1+ (or (position choice options) 0)) waspos))
            (:mpd-browse-previous
             (unless %interactivep%
               (mpd-browse-artists genre (or (car waspos) 0) (cdr waspos))))
            (:mpd-browse-next
               (mpd-browse-tracks choice artist 0 (cons (or (position choice options) 0) waspos)))))))

(defcommand mpd-browse-tracks (album &optional artist (startpos 0) waspos) ((:string "Album: "))
    (let* ((response (mpd-send-command
                      (format nil "find album \"~a\"" album)))
           (options (mpd-uniq-and-sort-list response :title)))
        (multiple-value-bind (action choice)
            (mpd-menu "Select track" options *mpd-browse-menu-map* startpos)
          (case action
            (:mpd-browse-add-and-quit
             (mpd-search-and-add-title choice t))
            (:mpd-browse-add
             (mpd-search-and-add-title choice t)
             (mpd-browse-tracks album artist (1+ (or (position choice options) 0)) waspos))
            (:mpd-browse-previous
             (unless %interactivep%
               (mpd-browse-albums artist () (or (car waspos) 0) (cdr waspos))))))))

(defcommand mpd-browse-database (&optional dir (startpos 0) waspos) ()
  (let* ((response (mpd-send-command
                    (if dir
                        (format () "lsinfo \"~a\"" dir) "lsinfo")))
         (basename (lambda (x) (subseq x (1+ (or (position #\/ x :from-end t) -1)))))
         (pathjoin (lambda (x) (if dir (concatenate 'string dir "/" x) x)))
         (dirs (mapcar basename (mpd-uniq-and-sort-list response :directory t t)))
         (files (mapcar basename (mpd-uniq-and-sort-list response :file t t)))
         (playlists (mapcar basename (mpd-uniq-and-sort-list response :playlist t t)))
         (options (concatenate 'list (mapcar (lambda (x) (cons x :directory)) dirs) (mapcar (lambda (x) (cons x :file)) files) (mapcar (lambda (x) (cons x :playlist)) playlists))))
    (multiple-value-bind (action choice)
        (mpd-menu "Select option" options *mpd-browse-menu-map* startpos)
      (case action
        (:mpd-browse-add-and-quit
         (if (equal :playlist (cdr choice))
             (mpd-format-command "load \"~a\"" (funcall pathjoin (car choice)))
           (mpd-add-file (funcall pathjoin (car choice)))))
        (:mpd-browse-add
         (if (equal :playlist (cdr choice))
             (mpd-format-command "load \"~a\"" (funcall pathjoin (car choice)))
           (mpd-add-file (funcall pathjoin (car choice))))
         (mpd-browse-database dir (1+ (or (position choice options) 0)) waspos))
        (:mpd-browse-previous
         (unless %interactivep%
           (mpd-browse-database (caar waspos) (cdar waspos) (cdr waspos))))
        (:mpd-browse-next
         (if (not (equal :directory (cdr choice)))
             (mpd-browse-database dir (or (position choice options) 0) waspos)
           (mpd-browse-database (funcall pathjoin (car choice)) 0 (cons (cons dir (or (position choice options) 0)) waspos))))))))


;;misc. commands
(defcommand mpd-connect () ()
  (message "~a" (init-mpd-connection)))

(defcommand mpd-disconnect () ()
  "Disconnect from mpd server"
  (with-mpd-connection
   (close *mpd-socket*)
   (setf *mpd-socket* nil)
   (when *mpd-timer* (cancel-timer *mpd-timer*))))

(defcommand mpd-kill () ()
 (mpd-send-command "kill"))

(defcommand mpd-toggle-pause () ()
  (mpd-update-status)
  (cond
   ((equal (mpd-get-status) "Playing")
    (mpd-send-command "pause 1"))
   ((equal (mpd-get-status) "Paused")
      (mpd-send-command "pause 0"))
   ((equal (mpd-get-status) "Stopped")
    (mpd-play))))

(defcommand mpd-toggle-random () ()
  (mpd-update-status)
  (if (mpd-shuffle-p)
      (mpd-send-command "random 0")
    (mpd-send-command "random 1")))

(defcommand mpd-toggle-repeat () ()
  (mpd-update-status)
  (if (mpd-repeating-p)
      (mpd-send-command "repeat 0")
    (mpd-send-command "repeat 1")))

;; If single is turned on we want to turn repeat off. (If we actually want to
;; repeat a single we can send the command manually. :P)
(defcommand mpd-toggle-single () ()
  (mpd-update-status)
  (if (mpd-single-p)
      (progn
	(if *mpd-did-repeat*
	    (mpd-send-command "repeat 1"))
	(mpd-send-command "single 0"))
    (progn
      (setf *mpd-did-repeat* (mpd-repeating-p))
      (mpd-send-command "repeat 0")
      (mpd-send-command "single 1"))))

(defvar *mpd-xfade-default* 5
  "The value to which to set crossfade by default.
Can be set in your rc or using `mpd-set-xfade' (this session only).")

(defcommand mpd-toggle-xfade () ()
  "Toggles crossfade. Uses `mpd-xfade-default' when turning crossfade on."
  (if (equal (assoc-value :xfade *mpd-status*) "0")
      (mpd-send-command (concat "crossfade "
                                (write-to-string *mpd-xfade-default*)))
    (mpd-send-command "crossfade 0")))

(defcommand mpd-set-xfade (xfade) ((:number "Fade: "))
  "Sets the crossfade to the specified value (in seconds).
Passed an argument of zero and if crossfade is on, toggles crossfade off."
  (unless (equal xfade 0)
    (setf *mpd-xfade-default* xfade))
  (mpd-send-command (concat "crossfade " (write-to-string xfade))))

(defcommand mpd-play () ()
  (mpd-send-command "play"))

(defcommand mpd-play-track (track) ((:number "Track: "))
  (mpd-format-command "play ~d"  track))

(defcommand mpd-stop () ()
  (mpd-send-command "stop"))

(defcommand mpd-next () ()
  (mpd-send-command "next"))

(defcommand mpd-prev () ()
  (mpd-send-command "previous"))

(defcommand mpd-set-volume (vol) ((:number "Set volume to: "))
  (mpd-send-command (format nil "setvol ~a" vol)))

(defcommand mpd-volume-up () ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (+ vol *mpd-volume-step*)))))

(defcommand mpd-volume-down () ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (- vol *mpd-volume-step*)))))

(defcommand mpd-clear () ()
  (mpd-send-command "clear"))

(defcommand mpd-update (&optional (path nil)) ()
  (if path
      (message "~a" (mpd-format-command "update ~a" path))
      (message "~a" (mpd-send-command "update"))))

(defcommand mpd-current-song () ()
  (mpd-update-current-song)
  (mpd-update-status)
  (message "~a"
	   (format-expand *mpd-formatters-alist* *mpd-current-song-fmt*)))

(defcommand mpd-status () ()
  (mpd-update-status)
  (mpd-update-current-song)
  (message "~a"
	   (format-expand *mpd-formatters-alist* *mpd-status-fmt*)))

(defun mpd-output-process (inlist)
  (let ((outlist ())
	(celem ()))
    (dolist (ie inlist)
      (if (equal (car ie) :file)
	  (progn
	    (if celem
		(setf outlist (append outlist (list celem))))
	    (setf celem ie))
	(setf celem (append celem ie))))
    (setf outlist (append outlist (list celem)))
    outlist))

(defun mpd-create-label (infunc)
  (let* ((title (getf infunc :title))
         (filename (getf infunc :file))
         (basename (subseq filename (1+ (position #\/ filename :from-end t)) (position #\. filename :from-end t)))
         (realname (if (null title) basename title)))
    realname))

(defcommand mpd-playlist () ()
  (let* ((response (mpd-output-process (mpd-send-command "playlistinfo")))
         (result (mapcar (lambda (infunc) 
   (list (mpd-create-label infunc)
 (read-from-string (let ((tv (getf infunc :time))) (if (null tv) "0" tv)))))
 response))
 ;; The field width which the song names should fill
 (mlen (max 50 (+ 10 (apply #'max (mapcar (lambda (istr) (length (car istr))) result)))))
 ;; Total playing length of the playlist
 (tplay (mpd-minutes-seconds (apply #'+ (mapcar #'cadr result)))))
    (message "Current playlist (~a, ~a): ~%^7*~{~{~:[~;^B~]~3d. ~Va~a~:[~;^b~]~%~}~}" ;; This is now far less horrible than my first version, which had a format-to-string
         (length result)                                                          ;; generating the format string for the format-to-output. *shudder*
         tplay
         (let* ((pn (1- (let ((n (mpd-get-number))) (if (null n) 0 (read-from-string n)))))
                (outlist (let ((cn 0))
                           (mapcar (lambda (in) 
                                     (setf cn (1+ cn))
                                     (list (= cn (+ 1 pn)) cn mlen (car in) (mpd-minutes-seconds (cadr in)) (= cn (+ 1 pn))))
                                   result))))
           (if (< (length outlist) 55)
               outlist
             (let* ((ii (max 0 (- pn 25)))
                    (ai (min (length outlist) (+ pn 25))))
               (if (< (- ai ii) 50)
                   (progn
                     (setf ii (max 0 (- ii (- 50 (- ai ii)))))
                     (setf ai (min (length outlist) (+ ai (- 50 (- ai ii)))))))
               (subseq outlist ii ai)))))))

(defcommand mpd-add-file (file) ((:rest "Add file to playlist: "))
  (mpd-format-command "add \"~a\"" file))

(defcommand mpd-remove-track (track-number) ((:number "Delete track number: "))
  (mpd-format-command "delete ~d" track-number))

(defcommand mpd-swap-tracks (track-1 track-2) ()
  (mpd-format-command "swap ~d ~d" track-1 track-2))

;;search and add commands
(defcommand mpd-search-and-add-artist (what &optional (exact-search nil))
  ((:rest "Search & add artist to playlist: "))
  (let* ((response (mpd-search "artist" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-file (what &optional (exact-search nil))
  ((:rest "Search & add file to playlist: "))
  (let* ((response (mpd-search "file" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-title (what &optional (exact-search nil))
  ((:rest "Search & add title to playlist: "))
  (let* ((response (mpd-search "title" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-album (what &optional (exact-search nil))
  ((:rest "Search & add album to playlist: "))
  (let* ((response (mpd-search "album" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-genre (what &optional (exact-search nil))
  ((:rest "Search & add genre to playlist: "))
  (let* ((response (mpd-search "genre" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

;;search commands
(defcommand mpd-search-artist (what) ((:rest "Search artist: "))
  (mpd-send-command (format nil "search artist \"~a\"" what)))
(defcommand mpd-search-file (what) ((:rest "Search file: "))
  (mpd-send-command (format nil "search file \"~a\"" what)))
(defcommand mpd-search-title (what) ((:rest "Search title: "))
  (mpd-send-command (format nil "search title \"~a\"" what)))
(defcommand mpd-search-album (what) ((:rest "Search album: "))
  (mpd-send-command (format nil "search album \"~a\"" what)))
(defcommand mpd-search-genre (what) ((:rest "Search genre: "))
  (mpd-send-command (format nil "search genre \"~a\"" what)))

(defvar *mpd-search-map* nil)
(defvar *mpd-browse-map* nil)
(defvar *mpd-add-map* nil)
(defvar *mpd-map* nil)

(defmacro fill-keymap (map &rest bindings)
  `(unless ,map
     (setf ,map
           (let ((m (make-sparse-keymap)))
             ,@(loop for i = bindings then (cddr i)
                    while i
                    collect `(define-key m ,(first i) ,(second i)))
             m))))

;;Key map
(fill-keymap *mpd-search-map*
             (kbd "a") "mpd-search-artist"
             (kbd "A") "mpd-search-album"
             (kbd "t") "mpd-search-title"
             (kbd "f") "mpd-search-file"
             (kbd "g") "mpd-search-genre")

(fill-keymap *mpd-browse-map*
             (kbd "p") "mpd-browse-playlist"
             (kbd "l") "mpd-browse-albums"
             (kbd "g") "mpd-browse-genres"
             (kbd "t") "mpd-browse-tracks"
             (kbd "a") "mpd-browse-artists")

(fill-keymap *mpd-add-map*
             (kbd "a") "mpd-search-and-add-artist"
             (kbd "A") "mpd-search-and-add-album"
             (kbd "t") "mpd-search-and-add-title"
             (kbd "f") "mpd-search-and-add-file"
             (kbd "g") "mpd-search-and-add-genre"
             (kbd "F") "mpd-add-file")

(fill-keymap *mpd-map*
             (kbd "SPC") "mpd-toggle-pause"
             (kbd "s") "mpd-toggle-random"
             (kbd "r") "mpd-toggle-repeat"
             (kbd "f") "mpd-toggle-xfade"
             (kbd "F") "mpd-set-xfade"
             (kbd "i") "mpd-current-song"
             (kbd "p") "mpd-play"
             (kbd "q") "mpd-browse-playlist"
             (kbd "o") "mpd-stop"
             (kbd "m") "mpd-next"
             (kbd "l") "mpd-prev"
             (kbd "c") "mpd-clear"
             (kbd "x") "mpd-connect"
             (kbd "k") "mpd-kill"
             (kbd "u") "mpd-update"
             (kbd "a") "mpd-search-and-add-artist"
             (kbd "z") "mpd-playlist"
             (kbd "v") "mpd-set-volume"
             (kbd "e") "mpd-volume-up"
             (kbd "d") "mpd-volume-down"
             (kbd "S") '*mpd-search-map*
             (kbd "b") '*mpd-browse-map*
             (kbd "A") '*mpd-add-map*)

;;; End of file

;;; "mpd" goes here. Hacks and glory await!
