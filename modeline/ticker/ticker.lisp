;;;; ticker.lisp

;;; Ticker formatter for the Stumpwm mode-line. There is no timestamp,
;;; so let's store up to some historical serie size values got from
;;; url and calculate its average. Comparing actual value with this
;;; average, set a color format. Adds a gauge control that draws
;;; tendency between low and high in 24 hours values.

;;; When creating a new ticker, it launches an asynchronous process
;;; that reads values every delay time from the API and stores them in
;;; the structure. The mode-line uses those structures to print the
;;; values on every refresh.

;;; CODE:

(in-package :ticker)

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
  (values ())           ; store the last 3 hours values
  (value 0.0)           ; last value got from url
  (values-low 0.0)      ; low value last 24h
  (values-high 0.0)     ; high value last 24h
  (values-average 0.0)) ; average last 3 hours values

;;; Global variables

(defparameter *tickers* ()
  "List of tickers to show.")

(defparameter *url* "https://api.kraken.com/0/public/Ticker?pair="
  "Location of price provider, the ticker pair will be concatenated.")

(defparameter *stop-parallel-getters* nil
  "When `t' stop and close all asynchronous loops that get the tickers
values.")

;;; Exported

(defun define-ticker (&key (pair "XXBTZUSD") (symbol "BTC") (colors t)
                        (threshold 0.001) (delay 30) (decimals 0)
                        (localization 2) (gauge-width 7))
  "Ticker constructor which defaults to Bitcoin and 3 hours historical values."
  (let ((ticker (make-ticker
                 :pair pair
                 :symbol symbol
                 :colors colors
                 :threshold threshold
                 :delay delay
                 :decimals decimals
                 :localization localization
                 :gauge-width gauge-width
                 ;; Internal state variable
                 :values (make-list (truncate (/ (* 3 60 60) ; 3 hours
                                                 delay))
                                    :initial-element NIL))))
    ;; Push the `ticker' into the `*tickers*' list, and launch the
    ;; asynchronous process that will update the values from the API
    ;; every `delay' seconds.
    (push ticker *tickers*)
    (let ((lparallel:*kernel*
            (lparallel:make-kernel 1 :name pair)))
      (lparallel:submit-task (lparallel:make-channel)
                             (lambda ()
                               (parallel-getter (car *tickers*)))))))

(defparameter *tickers-separator* " | "
  "String to separate between tickers in de modeline.")

;;; Get the values

(defun parallel-getter (tick)
  "The values are stored in the `*tickers*' structure, from where can be
read by the `ticker-modeline' function."
  (do ()
      (*stop-parallel-getters*
       (lparallel:end-kernel))
    ;; Store actual, 24h low, and 24h high values from the `*url*' API.
    ;; If there is no response, store just `nil' values.
    (let ((values
            (let* ((url (concatenate 'string *url* (ticker-pair tick)))
                   (response (handler-case
                                 (gethash (ticker-pair tick)
                                          (gethash "result"
                                                   (yason:parse
                                                    (dexador:get url
                                                                 :keep-alive nil))))
                               ;; Return NIL in case some condition is triggered
                               (condition () nil))))
              (if response
                  (list (read-from-string (first (gethash "c" response)))
                        (read-from-string (second (gethash "l" response)))
                        (read-from-string (second (gethash "h" response))))
                  (list nil nil nil)))))
      ;; From actual, 24 low, and 24h high, calculate average and
      ;; store all in the `*tickers*' ticker.
      (setf (ticker-value tick) (first values)
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
                                              (max 1 (length values-clean))))))
    ;; And again
    (sleep (ticker-delay tick))))

(defun purge-tickers ()
  "Stop the getters and reset the list of tickers."
  (let ((max-delay (reduce 'max
                           *tickers*
                           :key 'ticker-delay
                           :initial-value 0)))
    (setf *stop-parallel-getters* t)
    ;; Reset the flag to nil after some time
    (sleep (1+ max-delay))
    (setf *stop-parallel-getters* nil)
    ;; Reset the *tickers* list
    (setf *tickers* ())))

(defun reset-tickers ()
  "Reset the tickers, purging and restoring the list of tickers."
  (let ((tickers-backup (reverse *tickers*)))
    ;; Remove getters and *tickers* list
    (purge-tickers)
    ;; Restore the *tickers* list from its copy
    (mapcar (lambda (tk)
              (define-ticker
                :pair (ticker-pair tk)
                :symbol (ticker-symbol tk)
                :colors (ticker-colors tk)
                :threshold (ticker-threshold tk)
                :delay (ticker-delay tk)
                :decimals (ticker-decimals tk)
                :localization (ticker-localization tk)
                :gauge-width (ticker-gauge-width tk)))
            tickers-backup)))

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
by the `parallel-getter' function when the `delay' interval has been
reached. If there are not returned values from the API (nil), then the
ticker name is printed."
  (declare (ignore ml))
  (if *tickers*
      (let ((results ()))
        (dolist (tick *tickers*)
          (if (and (numberp (ticker-value tick)) (plusp (ticker-value tick)))
              ;; Actual value is a positive number, so print off
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
              ;; The value is not a positive number, set the tick name as response
              (push (format nil "-~A-" (ticker-pair tick)) results)))
        ;; Return aggregated ticks results with proper separator
        (let ((s (concatenate 'string "~{~A~^" *tickers-separator* "~}")))
          (format nil s results)))
      ;; There are no tickers defined
      "-Ticker-"))

;; Bind modeline formatter character to the drawer function
(stumpwm:add-screen-mode-line-formatter #\T 'ticker-modeline)
