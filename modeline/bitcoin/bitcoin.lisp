;;;; bitcoin.lisp

(in-package :bitcoin)

;;; CODE:

;;; Bitcoin formatter for the Stumpwm mode-line.
;;; There is no timestamp, so let's store up to size last
;;; values got from url and calculate its average. Comparing
;;; actual value with its average, set a color format.

;;; Exported

(defparameter *modeline-use-colors* t
  "Flag use color or not in price value.")

(defparameter *threshold* 0.001
  "Magnitude that must be exceeded for a increasing or decreasing color.")

(defparameter *time-delay* 15
  "Time in seconds between calls to `*url*' to get price.")

;;; Get price

(defparameter *url* "https://api.coindesk.com/v1/bpi/currentprice.json"
  "Location of price provider.")

(defvar *prev-time* 0
  "Store previous time when got price.")

(defvar *prev-value* 0.0
  "Store previous price. ")

(defun bitcoin-restart (c)
  "Define a restart point to handle when *any* condition is signaled."
  (declare (ignore c))
  (invoke-restart 'bitcoin-restarter))

(defun get-value-from-url ()
  "Get the actual USD-BTC value."
  ;; Just in case internet drops
  (handler-bind ((t #'bitcoin-restart))
    (restart-case
        (gethash "rate_float"
                 (gethash "USD"
                          (gethash "bpi"
                                   (yason:parse
                                    (babel:octets-to-string
                                     (dexador:get *url*)
                                     :encoding :utf-8)))))
      (bitcoin-restarter () 0.0))))

(defun get-value-with-delay ()
  "Get price from `*url*' only once every `*time-delay'."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    ;; When `*time-delay*' time passed, ask for new price through internet
    ;; and update `*prev-value*'.
    (when (> (- now *prev-time*) *time-delay*)
      (progn (setf *prev-time* now)
             (setf *prev-value* (get-value-from-url))))
    *prev-value*))

;;; Store prices

(defvar *last-values-size* 420
  "Size of last values stored list. The value is get from `*url*' on every
modeline refresh, so depends on user's swapping windows behavior to set
proprerly, with the `*time-delay*' time influencing too. Must be positive,
is used in average price calculation.")

(defvar *last-values*
  (make-list *last-values-size*
             :initial-element (get-value-with-delay))
  "List of last values got from `*url*'. Set size to `*last-values-size*'
and initial element to actual value from url.")

;;; Write on modeline

(defun bitcoin-modeline (ml)
  "Get the actual USD-BTC value, store value in list, preserve list size
popping first value, calculate average and set formatting depending on
value vs average. This function is evaluated on every modeline refresh."
  (declare (ignore ml))
  (let ((value (get-value-with-delay))
        (last-values-average 0.0))
    ;; Actual value must be positive number
    (if (and (numberp value) (plusp value))
        (progn
          ;; Add value to last values list, appending to end
          (setf *last-values* (append *last-values* (list value)))
          ;; Preserve last values list at size, popping from front
          (pop *last-values*)
          ;; Calculate average of last values
          (setf last-values-average (/ (reduce #'+ *last-values*)
                                       *last-values-size*))
          ;; Return with color if desired
          (if *modeline-use-colors*
              (let* ((diff (- value last-values-average))
                     (pdiff (/ diff value)))
                (cond ((> pdiff *threshold*)
                       (format nil "^[^B^3*~1$^]" value))
                      ((< pdiff (- *threshold*))
                       (format nil "^[^1*~1$^]" value))
                      (t (format nil "^[^7*~1$^]" value))))
              (format nil "^[^**~1$^]" value)))
        (format nil "-BTC-"))))

(stumpwm:add-screen-mode-line-formatter #\b 'bitcoin-modeline)

;;; Debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (declaim (optimize (speed 0) (debug 3) (safety 0)))

;;; CL-USER > (ql:quickload :bitcoin)
;;; CL-USER > (in-package "BITCOIN")
;;; BITCOIN > (do () (nil)
;;;             (let* ((price (get-value-with-delay))
;;;                    (average (/ (reduce #'+ *last-values*)
;;;                                *last-values-size*))
;;;                    (diff (- price average)))
;;;               (format t "~&~2$ ~2$ ~2@$ ~4@$% ~a"
;;;                       price
;;;                       average
;;;                       diff
;;;                       (* 100 (/ diff price))
;;;                       (bitcoin-modeline t)))
;;;             (force-output)
;;;             (sleep 1))

;;; (do () (nil)
;;;   (let ((price (get-value-with-delay))
;;;         (average 0.0)
;;;         (diff 0.0)
;;;         (ratio 0.0))
;;;     (when  (and (numberp price) (plusp price))
;;;       (setf average (/ (reduce #'+ *last-values*)
;;;                        *last-values-size*))
;;;       (setf diff (- price average))
;;;       (setf ratio (* 100 (/ diff price))))
;;;     (format t "~&~2$ ~2$ ~2@$ ~4@$%"
;;;             price
;;;             average
;;;             diff
;;;             ratio))
;;;   (force-output)
;;;   (sleep 1))

;;; (do () (nil)
;;;   (format t "~&~a" (bitcoin-modeline t))
;;;   (force-output)
;;;   (sleep 1))

;;; Wile executing, swap to code buffers, and any re-compile-load-ed
;;; changes will be visible.
