;;;; amixer.lisp

(in-package #:amixer)

;;; Amixer module for StumpWM.
;;;
;;; Copyright 2007 Amy Templeton, Jonathan Moore Liles, Ivy Foster.
;;;
;;; Maintainer: Ivy Foster
;;;
;;; Code:

(defun volcontrol (device channel amount)
  (let* ((output (run-shell-command
                  (concat "amixer -D "
                          (or device "default")
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

(defvolcontrol amixer-Front-1- "Front" "1%-")
(defvolcontrol amixer-Front-1+ "Front" "1%+")
(defvolcontrol amixer-Front-toggle "Front" "toggle")

(defvolcontrol amixer-Master-1- "Master" "1%-")
(defvolcontrol amixer-Master-1+ "Master" "1%+")
(defvolcontrol amixer-Master-toggle "Master" "toggle")

(defvolcontrol amixer-Headphone-1- "Headphone" "1%-")
(defvolcontrol amixer-Headphone-1+ "Headphone" "1%+")
(defvolcontrol amixer-Headphone-toggle "Headphone" "toggle")

(defcommand amixer-sense-toggle () ()
  (message
   (concat "Headphone Jack Sense toggled"
           (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t))))

;;; End of file
