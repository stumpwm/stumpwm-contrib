;;;; ticker.lisp

(in-package :ticker)

;;; CODE:

;;; Ticker formatter for the Stumpwm mode-line. There is no timestamp,
;;; so let's store up to some historical serie size values got from
;;; url and calculate its average. Comparing actual value with this
;;; average, set a color format. Adds a gauge control that draws
;;; tendency between low and high in 24 hours values.

(defstruct ticker
  "Parameters of the ticker and state variables."
  pair         ; get from API url
  symbol       ; to show in modeline
  colors       ; show colors
  threshold    ; color change interval
  delay        ; update interval
  decimals     ; digits in decimal part
  localization ; thousands/comma format
  gauge-width  ; width of gauge in characters
  ;; Internal state variables
  (values ())          ; store the last 3 hours values
  (value 0.0)          ; last value got from url
  (values-low 0.0)     ; low value last 24h
  (values-high 0.0)    ; high value last 24h
  (values-average 0.0) ; average last 3 hours values
  (prev-time 0))       ; store last update time

(defparameter *tickers* ()
  "List of tickers to show.")

;;; Exported

(defun define-ticker (&key (pair "XXBTZUSD") (symbol "BTC") (colors t)
                        (threshold 0.001) (delay 30) (decimals 0)
                        (localization 2) (gauge-width 7))
  "Ticker constructor which defaults to Bitcoin and 3 hours historical values."
  (setf *tickers*
        (append *tickers*
                (list (make-ticker
                       :pair pair
                       :symbol symbol
                       :colors colors
                       :threshold threshold
                       :delay delay
                       :decimals decimals
                       :localization localization
                       :gauge-width gauge-width
                       ;; Internal state variables
                       :values (make-list (truncate (/ (* 3 60 60) ; 3 hours
                                                       delay))
                                          :initial-element NIL))))))

(defparameter *tickers-separator* " | "
  "String to separate between tickers in de modeline.")

;;; Get price

(defparameter *url* "https://api.kraken.com/0/public/Ticker?pair="
  "Location of price provider, the ticker pair will be concatenated.")

(defun get-values-from-url (tick)
  "Get the actual, 24h low and 24h high values from the `*url*' API."
  (let* ((url (concatenate 'string *url* (ticker-pair tick)))
         (response (handler-case
                       (gethash (ticker-pair tick)
                                (gethash "result"
                                         (yason:parse
                                          (dexador:get url
                                                       :keep-alive nil))))
                     ;; Return NIL in case some condition is triggered
                     (condition () nil))))
    (unless (null response)
      (list (read-from-string (first (gethash "c" response)))
            (read-from-string (second (gethash "l" response)))
            (read-from-string (second (gethash "h" response)))))))

(defun refresh-values (tick)
  "Refresh values from `*url*' if the `:delay' has been reached.
Get the actual `:pair' value, store value in list, preserve list size
popping first value, and calculate average."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second))
        (values (get-values-from-url tick)))
    (when (> (- now (ticker-prev-time tick)) (ticker-delay tick))
      (progn
        (setf (ticker-prev-time tick) now
              (ticker-value tick) (first values)
              (ticker-values-low tick) (second values)
              (ticker-values-high tick) (third values))
        ;; Add value to values list, pushing to front
        (push (ticker-value tick) (ticker-values tick))
        ;; Preserve values list size, popping from end
        (setf (ticker-values tick) (nreverse (ticker-values tick)))
        (pop (ticker-values tick))
        (setf (ticker-values tick) (nreverse (ticker-values tick)))
        ;; Calculate average of values, excluding NIL values
        ;; that could exist because network issues.
        (let ((values-clean (remove-if-not #'numberp (ticker-values tick))))
          (setf (ticker-values-average tick) (/ (reduce #'+ values-clean)
                                                (max 1 (length values-clean)))))))))

;;; Write on modeline

(defun format-decimal (n sep int com dec)
  "Return Number formated in groups of INTerval length every, and
separated by SEParator, with COMma character as decimal separator.
DECimals is the number of digits in the decimal part. All parameters
but N are strings. COMma character should not be the tilde `~'.

Works as a simple formatting positive numbers using directive `~D',
for thousand separator and direct value displacement in the decimal
part. Uses `truncate' so there is some precission loss. Does NOT work
with negative numbers.

Based on https://stackoverflow.com/questions/35012859"
  (let* ((num-string (concatenate 'string "~,,'" sep "," int ":D"))
         (decimals (format nil "~D" dec))
         (dec-string (concatenate 'string com "~" decimals ",'0D")))
    (multiple-value-bind (i r) (truncate n)
      (concatenate
       'string
       (format nil num-string i)
       (when (< 0 dec)
         (format nil dec-string (truncate (* (expt 10 dec) r))))))))

(defun gauge (v l h n)
  "Draw a gauge control with Value at the point between Low and High in
an N length control."
  (if (and (< l h) (<= l v) (<= v h) (> n 1))
      (let* ((line (make-sequence 'string n :initial-element #\-))
             (segment (floor (* n (/ (- v l) (- h l)))))
             (segment (if (= v h) (1- segment) segment)))
        (replace line "*" :start1 segment))
      "-*-*-"))

(defun get-value-string (tick)
  "Generate the ticker string to show in modeline."
  (let ((results ()))
    (when (< 0 (length (ticker-symbol tick)))
      (push (ticker-symbol tick) results))
    (push (case (ticker-localization tick)
            (0 (format nil "~,2F" (ticker-value tick)))
            (1 (format-decimal (ticker-value tick) "," "3" "."
                               (ticker-decimals tick)))
            (2 (format-decimal (ticker-value tick) "." "3" ","
                               (ticker-decimals tick)))
            (3 (format-decimal (ticker-value tick) " " "3" ","
                               (ticker-decimals tick)))
            (otherwise (format nil "~,2F" (ticker-value tick))))
          results)
    (when (< 1 (ticker-gauge-width tick))
      (push (gauge (ticker-value tick)
                   (ticker-values-low tick)
                   (ticker-values-high tick)
                   (ticker-gauge-width tick))
            results))
    (format nil "~{~A~^ ~}" (nreverse results))))

(defun ticker-modeline (ml)
  "This function is evaluated on every modeline refresh and returns the
modeline string. The values are always printed off, but only updated
when the `refresh-values' function allows it through the `:threshold'
parameter."
  (declare (ignore ml))
  (if *tickers*
      (let ((results ()))
        (dolist (tick *tickers*)
          (refresh-values tick)
          ;; Actual value must be positive number
          (if (and (numberp (ticker-value tick)) (plusp (ticker-value tick)))
              ;; Apply desired format to value
              (let ((value-string (get-value-string tick)))
                ;; Return with color if desired
                (push (if (ticker-colors tick)
                          (let* ((diff (- (ticker-value tick) (ticker-values-average tick)))
                                 (pdiff (/ diff (max 1 (ticker-value tick)))))
                            (cond ((> pdiff (ticker-threshold tick))
                                   (format nil "^[^B^3*~A^]" value-string))
                                  ((< pdiff (- (ticker-threshold tick)))
                                   (format nil "^[^1*~A^]" value-string))
                                  (t (format nil "^[^7*~A^]" value-string))))
                          (format nil "^[^**~A^]" value-string))
                      results))
              ;; The value is not a positive number
            (format nil "-~A-" (ticker-pair tick))))
        ;; Return aggregated results with proper separator
        (let ((s (concatenate 'string "~{~A~^" *tickers-separator* "~}")))
          (format nil s (nreverse results))))
    ;; There are no tickers defined
    "-Ticker-"))

;; Bind modeline formatter character to the drawer function
(stumpwm:add-screen-mode-line-formatter #\T 'ticker-modeline)
