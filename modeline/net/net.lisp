;;;; net.lisp

(in-package #:net)

;;; "net" goes here. Hacks and glory await!

;;; Network activity formatter for the mode-line
;;;
;;; Copyright 2009 Vitaly Mayatskikh
;;;
;;; Maintainer:
;;;

;; Install formatters.
(add-screen-mode-line-formatter #\l 'net-modeline)

(defvar *net-device* nil) ; nil means auto. or specify explicitly, i.e. "wlan0"
(defvar *net-ipv4* nil)
(defvar *net-ipv6* nil)
(defvar *net-last-rx* 0)
(defvar *net-last-tx* 0)
(defvar *net-last-time* nil)
(defvar *net-rx* nil)
(defvar *net-tx* nil)
(defvar *net-time* nil)

;; stuff for parsing /proc/net/route
(defconstant +iface+ 0)
(defconstant +destination+ 1)
(defconstant +gateway+ 2)
(defconstant +flags+ 3)
(defconstant +mask+ 7)
(handler-bind
    #+sbcl ((SB-EXT:DEFCONSTANT-UNEQL
                #'(lambda (condition) (continue))))
    (defconstant +ipv4-zero+ "00000000"))

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defvar *last-route-rescan-time* (now))
(defvar *last-route-device* nil)

(defun find-default ()
  "Tries to found device with default route. NIL if none."
  (with-open-file (file #P"/proc/net/route" :if-does-not-exist nil)
    (when file
      (read-line file nil) ; skip desc
      (loop :as line = (read-line file nil)
	 :when (null line) :return nil
	 :do
	 (let ((split (cl-ppcre:split "\\s+" line)))
	   (when (and (string= (nth +destination+ split) +ipv4-zero+)
		      (string= (nth +mask+ split) +ipv4-zero+)
		      (logand (parse-integer (nth +flags+ split) :junk-allowed t) 2))
	     (return (nth +iface+ split))))))))

(defun net-device ()
  "Returns statically assigned device name or tries to find it be default gw.
For the second case rescans route table every minute."
  (if *net-device*
      *net-device*
      (if (and *last-route-device*
	       (< (- (now) *last-route-rescan-time*) 60))
	  *last-route-device*
	  (let ((new-device (or (find-default) "lo")))
	    (when (string/= new-device *last-route-device*)
              (setf *net-ipv4*
                    (string-trim '(#\Newline)
                     (run-shell-command
                      (format nil
                       "/sbin/ip -o -4 addr list ~A | awk '{print $4}' | cut -d/ -f1" new-device)
                     t)))
              (setf *net-ipv6*
                    (string-trim '(#\Newline)
                     (run-shell-command
                      (format nil
                       "/sbin/ip -o -6 addr list ~A | awk '{print $4}' | cut -d/ -f1" new-device)
                     t)))
	      (setq *net-last-tx* 0
		    *net-last-rx* 0
		    *net-last-time* nil
		    *net-rx* nil
		    *net-tx* nil
		    *net-time* nil))
	    (setq *last-route-rescan-time* (now)
		  *last-route-device* new-device)))))

(defun net-sys-stat-read (device stat-file)
  (with-open-file (file (concatenate 'string "/sys/class/net/"
				     device
				     "/statistics/"
				     stat-file) :if-does-not-exist nil)
    (if file
	(parse-integer (read-line-from-sysfs file) :junk-allowed t)
	(progn (setq *net-device* nil
		     *last-route-device* nil)
	       0))))

(defun net-usage ()
  "Returns a list of 2 values: rx and tx bytes/second."
  (let ((now (now))
	(rx-s 0.0)
	(tx-s 0.0)
	(t-s 0.1) ; don't want division by zero
	(rx (net-sys-stat-read (net-device) "rx_bytes"))
	(tx (net-sys-stat-read (net-device) "tx_bytes")))

    (when (and *net-last-time* (> (- now *net-last-time*) 0.0))
      (let ((drx (/ (- rx *net-last-rx*)
		    (- now *net-last-time*)))
	    (dtx (/ (- tx *net-last-tx*)
		    (- now *net-last-time*))))

	(push drx *net-rx*)
	(push dtx *net-tx*)
	(push now *net-time*)

	(when (> (length *net-time*) 1)
	  (dotimes (i (1- (length *net-time*)))
	    (let ((dt (- (nth (1+ i) *net-time*)
			 (nth i *net-time*)))
		  (rx (nth i *net-rx*))
		  (tx (nth i *net-tx*)))
	      (incf rx-s (* rx dt))
	      (incf tx-s (* tx dt))
	      (incf t-s dt)))
	  ;; cut off old values
	  (when (> (length *net-time*) 5)
	    (pop *net-rx*)
	    (pop *net-tx*)
	    (pop *net-time*)))))

      (setq *net-last-rx* rx
	    *net-last-tx* tx
	    *net-last-time* now)

      (list (round (/ rx-s t-s))
	    (round (/ tx-s t-s)))))

(defun fmt-net-usage ()
  "Returns a string representing the current network activity."
  (let ((net (net-usage))
	      dn up)
    (defun kbmb (x y)
      (if (>= (/ x 1e6) y)
	        (list (/ x 1e6) "m")
	        (list (/ x 1e3) "k")))
    (setq dn (kbmb (car net) 0.1)
	        up (kbmb (cadr net) 0.1))
    (format nil "~5,2F~A/~5,2F~A"
            (car dn) (cadr dn) (car up) (cadr up))))

(defun fmt-ipv4 ()
  (or *net-ipv4* "noip"))

(defun fmt-ipv6 ()
  (or *net-ipv6* "noip"))

(defun net-modeline (ml)
  (declare (ignore ml))
  (format-expand *net-formatters-alist*
                 *net-modeline-fmt*))

(defvar *net-formatters-alist*
  '((#\d  net-device)
    (#\u  fmt-net-usage)
    (#\i  fmt-ipv4)
    (#\I  fmt-ipv6)))

(defvar *net-modeline-fmt* "%d: %u"
  "The default value for displaying net information on the modeline.

@table @asis
@item %%
A literal '%'
@item %d
network device name
@item %u
network usage
@end table
")
