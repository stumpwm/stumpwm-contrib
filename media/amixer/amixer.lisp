;;;; amixer.lisp

(in-package #:amixer)

;;; Amixer module for StumpWM.
;;;
;;; Copyright 2007 Amy Templeton, Jonathan Moore Liles, Ivy Foster.
;;;
;;; Maintainer: Ivy Foster
;;;
;;; Code:
(defvar *default-device* 0)

(defun volcontrol (device channel amount)
  (let* ((output (run-shell-command
                  (concat "amixer -c "
                          (write-to-string (or device *default-device*))
                          " sset "
                          channel
                          " "
                          (or amount "toggle"))
                  t))
         (percent-status (nth-value 1
                                    (cl-ppcre:scan-to-strings ".*\\[([0-9]+)%\\].*\\[(on|off)\\]\\n"
                                                              output)))
         (percent (if (string= (aref percent-status 1) "off")
                      0
                      (parse-integer (aref percent-status 0)))))
    (message
     (concat "Mixer: " channel " " (or amount "toggled")
             (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
             (bar percent 50 #\# #\:) "^]]"))))

(defmacro defvolcontrol (name channel valence)
  `(defcommand ,name (&optional device) ((:string))
     (volcontrol device ,channel ,valence)))

(defvolcontrol amixer-PCM-1- "PCM" "1%-")
(defvolcontrol amixer-PCM-1+ "PCM" "1%+")
(defvolcontrol amixer-PCM-toggle "PCM" "toggle")
(defvolcontrol amixer-PCM-mute "PCM" "mute")
(defvolcontrol amixer-PCM-unmute "PCM" "unmute")

(defvolcontrol amixer-Front-1- "Front" "1%-")
(defvolcontrol amixer-Front-1+ "Front" "1%+")
(defvolcontrol amixer-Front-toggle "Front" "toggle")
(defvolcontrol amixer-Front-mute "Front" "mute")
(defvolcontrol amixer-Front-unmute "Front" "unmute")

(defvolcontrol amixer-Master-1- "Master" "1%-")
(defvolcontrol amixer-Master-1+ "Master" "1%+")
(defvolcontrol amixer-Master-toggle "Master" "toggle")
(defvolcontrol amixer-Master-mute "Master" "mute")
(defvolcontrol amixer-Master-unmute "Master" "unmute")

(defvolcontrol amixer-Headphone-1- "Headphone" "1%-")
(defvolcontrol amixer-Headphone-1+ "Headphone" "1%+")
(defvolcontrol amixer-Headphone-toggle "Headphone" "toggle")
(defvolcontrol amixer-Headphone-mute "Headphone" "mute")
(defvolcontrol amixer-Headphone-unmute "Headphone" "unmute")

(defcommand amixer-sense-toggle () ()
  (message
   (concat "Headphone Jack Sense toggled"
           (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t))))

;;; End of file
