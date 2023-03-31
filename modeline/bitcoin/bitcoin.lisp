;;;; bitcoin.lisp

(in-package :bitcoin)

;;; CODE:

;;; Bitcoin formatter for the Stumpwm mode-line.
;;; There is no timestamp, so let's store up to size 
;;; values got from url and calculate its average. Comparing
;;; actual value with this average, set a color format.

;;; Exported

(defparameter *modeline-use-colors* t
  "Flag use color or not in price value.")

(defparameter *threshold* 0.001
  "Magnitude that must be exceeded for a increasing or decreasing
color.")

(defparameter *time-delay* 30
  "Time in seconds between calls to `*url*' to get price. Must be
positive because `*values-size*'.")

(defparameter *local-code* 2
  "Localization code, `0' gives 1234.56, `1' gives 1,234.56, `2' gives
1.234,56, and `3' gives 1 234,56.")

(defparameter *decimals* 2
  "Number of decimals, set to `0' for no decimals.")

(defparameter *modeline-gauge* t
  "Show the tendency graphical gauge.")

(defparameter *gauge-width* 9
  "Width of the graphical gauge in characters. Must be greater than 1.")

;;; Global variables

(defvar *values*
  (make-list (truncate (/ (* 3 60 60) *time-delay*)) ; 3 hours
             :initial-element NIL)
  "List of values got from `*url*'. Set size to a list of the last n
hours getting values: a new coin value is appended in `*values*' every
`*time-delay*', so it is divided the desired n time in seconds by the
time-delay in seconds.")

(defvar *value* 0.0
  "Last value got from `*url*'.")

(defvar *values-low* 0.0
  "The low value in `*values*'.")

(defvar *values-high* 0.0
  "The high value in `*values*'.")

(defvar *values-average* 0.0
  "Average of values in `*values*'.")

(defvar *initialized* nil
  "When not nil the lparallel kernel has been initialized.")

;;; Get price

(defparameter *url* "https://api.kraken.com/0/public/Ticker?pair=xbtusd"
  "Location of price provider.")

(defun get-values-from-url ()
  "Get the USD-BTC, 24h LOW and 24h HIGH values."
  (let ((response (handler-case
                      (gethash "XXBTZUSD"
                               (gethash "result"
                                        (yason:parse
                                         (dexador:get *url*
                                                      :keep-alive nil))))
                    ;; Return NIL in case some condition is triggered
                    (condition () nil))))
    (unless (null response)
      (list (read-from-string (first (gethash "c" response)))
            (read-from-string (second (gethash "l" response)))
            (read-from-string (second (gethash "h" response)))))))

(defun refresh-values ()
  "Refresh values from `*url*' if the `*time-delay*' has been reached.
Get the actual USD-BTC value, store value in list, preserve list size
popping first value, calculate average and set formatting depending on
value vs average."
  (do ()
      (nil)
    (let ((values (get-values-from-url)))
      (setf *value* (first values)
            *values-low* (second values)
            *values-high* (third values)))
    ;; Add value to values list, pushing to front
    (push *value* *values*)
    ;; Preserve values list size, popping from end
    (setf *values* (nreverse *values*))
    (pop *values*)
    (setf *values* (nreverse *values*))
    ;; Calculate average of values, excluding NIL values
    ;; that could exist because network issues.
    (let ((values-clean (remove-if-not #'numberp *values*)))
      (setf *values-average* (/ (reduce #'+ values-clean)
                                (max 1 (length values-clean)))))
    (sleep *time-delay*)))

;;; Write on modeline

;;; Simple format positive numbers, using directive D for thousand
;;; separator and direct value displacement in the decimal part.  Uses
;;; truncate, so there is some precission loss, e.g. (truncate
;;; 1231231.0999) gives 1231231 and 0.125. Does NOT work with negative
;;; numbers.
;;; More in https://stackoverflow.com/questions/35012859
(defun format-decimal (n sep int com)
  "Return Number formated in groups of INTerval length every and
separated by SEParator, with COMma character as decimal separator. The
number of digits in the decimal part is defined by the global
parameter `*decimals*'. All parameters but N are strings. COMma
character should not be the tilde `~'."
  (let* ((num-string (concatenate 'string "~,,'" sep "," int ":D"))
         (decimals (format nil "~D" *decimals*))
         (dec-string (concatenate 'string com "~" decimals ",'0D")))
    (multiple-value-bind (i r) (truncate n)
      (concatenate
       'string
       (format nil num-string i)
       (when (< 0 *decimals*)
         (format nil dec-string (truncate (* (expt 10 *decimals*) r))))))))

(defun gauge (v l h n)
  "Draw a gauge control with Value at the point between Low and High in
an N length control."
  (if (and (< l h) (<= l v) (<= v h) (> n 1))
      (let* ((line (make-sequence 'string n :initial-element #\-))
             (segment (floor (* n (/ (- v l) (- h l)))))
             (segment (if (= v h) (1- segment) segment)))
        (replace line "*" :start1 segment))
      "-*-*-"))

(defun bitcoin-modeline (ml)
  "This function is evaluated on every modeline refresh and defines
the modeline string, so the values exist as global variables and are
updated with the `refresh-values' function."
  (declare (ignore ml))
  ;; Launch asynchronous process to capture values
  (unless *initialized*
    (setf *initialized* t)
    (let ((lparallel:*kernel*
            (lparallel:make-kernel 1 :name "bitcoin-kernel")))
      (lparallel:submit-task (lparallel:make-channel)
                             (lambda ()
                               (refresh-values)))))
  ;; Actual value must be positive number
  (if (and (numberp *value*) (plusp *value*))
      ;; Apply desired format to value
      (let ((value-string
              (concatenate
               'string
               (case *local-code*
                 (0 (format nil "~,2F" *value*))
                 (1 (format-decimal *value* "," "3" "."))
                 (2 (format-decimal *value* "." "3" ","))
                 (3 (format-decimal *value* " " "3" ","))
                 (otherwise (format nil "~,2F" *value*)))
               (when *modeline-gauge*
                 (concatenate
                  'string
                  " "
                  (gauge *value* *values-low* *values-high* *gauge-width*))))))
        ;; Return with color if desired
        (concatenate
         'string
         (if *modeline-use-colors*
             (let* ((diff (- *value* *values-average*))
                    (pdiff (/ diff (max 1 *value*))))
               (cond ((> pdiff *threshold*)
                      (format nil "^[^B^3*~A^]" value-string))
                     ((< pdiff (- *threshold*))
                      (format nil "^[^1*~A^]" value-string))
                     (t (format nil "^[^7*~A^]" value-string))))
             (format nil "^[^**~A^]" value-string))))
      ;; The value is not a positive number
      (format nil "-BTC-")))

(stumpwm:add-screen-mode-line-formatter #\b 'bitcoin-modeline)
