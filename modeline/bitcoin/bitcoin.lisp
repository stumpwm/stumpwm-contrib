;;;; bitcoin.lisp

(in-package :bitcoin)

;;; "bitcoin" goes here. Hacks and glory await!

;;; Display bitcoin price on StumpWM modeline.
;;;
;;; Copyright 2020 Santiago Payà Miralta @santiagopim.
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

;;; CODE:

;;; Bitcoin formatter for the Stumpwm mode-line.
;;; There is no timestamp, so let's store up to size last
;;; values got from url and calculate its average. Comparing
;;; actual value with its average, set a color format.

(add-screen-mode-line-formatter #\b 'bitcoin-modeline)

(defparameter *url* "https://api.coindesk.com/v1/bpi/currentprice.json"
  "Location of price provider.")

(defparameter *modeline-use-colors* t
  "Flag use color or not in price value.")

(defparameter *last-values* ()
  "List of last values got from *url*.")

(defparameter *last-values-size* 42
  "Size of last values stored list. The value is calculated on every
modeline refresh, so depends on user's behavior to set proprerly.")

(defun bitcoin-modeline (ml)
  "Get the actual USD-BTC value, store value in list, preserve list size
popping last value, calculate average and set formatting depending on
value vs average. This function is evaluated on every modeline refresh."
  (declare (ignore ml))
  (ignore-errors                        ; Just in case internet drops
   (let ((value (/ (gethash "rate_float"
                            (gethash "USD"
                                     (gethash "bpi"
                                              (yason:parse
                                               (babel:octets-to-string
                                                (dex:request *url*)
                                                :encoding :utf-8)))))
                   1000))
         (last-values-length (length *last-values*))
         (last-values-average 0))
     ;; Add value to last values list
     (push value *last-values*)
     ;; Preserve last values list at size
     (when (>= last-values-length *last-values-size*)
       (setf *last-values* (nreverse *last-values*))
       (pop *last-values*)
       (setf *last-values* (nreverse *last-values*)))
     ;; Calculate average of last values
     (setf last-values-average (/ (reduce #'+ *last-values*)
                                  last-values-length))
     ;; Return with color if 
     (if *modeline-use-colors*
         (cond ((> value last-values-average)
                (format nil "^[^B^3*~3$^]" value))
               ((< value last-values-average)
                (format nil "^[^1*~3$^]" value))
               (t (format nil "^[^7*~3$^]" value)))
         (format nil "^[^**~3$^]" value)))))
