;;;; logitech-g15-keysyms.lisp

(in-package #:logitech-g15-keysyms)

;;; "logitech-g15-keysyms" goes here. Hacks and glory await!

;;; keysyms for the Logitech G15 keyboard
;;;
;;; Copyright 2008 Ted Zlatanov
;;;
;;; Maintainer: 
;;;

(defun g15-original ()
  (define-keysym #x15000001 "G1")
  (define-keysym #x15000002 "G2")
  (define-keysym #x15000003 "G3")
  (define-keysym #x15000004 "G4")
  (define-keysym #x15000005 "G5")
  (define-keysym #x15000006 "G6")
  (define-keysym #x15000007 "G7")
  (define-keysym #x15000008 "G8")
  (define-keysym #x15000009 "G9")
  (define-keysym #x15000010 "G10")
  (define-keysym #x15000011 "G11")
  (define-keysym #x15000012 "G12")
  (define-keysym #x15000013 "G13")
  (define-keysym #x15000014 "G14")
  (define-keysym #x15000015 "G15")
  (define-keysym #x15000016 "G16")
  (define-keysym #x15000017 "G17")
  (define-keysym #x15000018 "G18")
  (define-keysym #x15000019 "M1")
  (define-keysym #x1500001a "M2")
  (define-keysym #x1500001b "M3")
  (define-keysym #x1500001d "LCD0")
  (define-keysym #x1500001e "LCD1")
  (define-keysym #x1500001f "LCD2")
  (define-keysym #x15000020 "LCD3")
  (define-keysym #x15000021 "LCD4"))

(defun g15-revised () ;; the orange version
  (let ((gkeycodes '(177 152 190 208 129 178)) ;; These are the keycodes G keys in order
	(mediakeycodes '((144 "Prev") (153 "Next") (160 "Mute") (162 "Play")
			 (164 "Stop") (174 "LowerVolume") (176 "RaiseVolume"))))
    (dotimes (i (length gkeycodes))
      (run-shell-command
       (concatenate 'string
		    "xmodmap -e 'keycode "
		    (write-to-string (nth i gkeycodes))
		    " = XF86Launch"
		    (write-to-string (1+ i))
		    "'"))
      (define-keysym (+ (keysym-name->keysym "XF86Launch1") i)
	  (concatenate 'string
		       "G"
		       (write-to-string (1+ i)))))
    (dotimes (i (length mediakeycodes))
      (run-shell-command
       (concatenate 'string
		    "xmodmap -e 'keycode "
		    (write-to-string (first (nth i mediakeycodes)))
		    " = XF86Audio"
		    (second (nth i mediakeycodes))
		    "'")))))
