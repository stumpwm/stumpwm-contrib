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

;;; Get price

(defparameter *url* "https://api.coindesk.com/v1/bpi/currentprice.json"
  "Location of price provider.")

(defvar *prev-time* 0
  "Store previous time when got price.")

(defun get-value-from-url ()
  "Get the actual USD-BTC value."
  ;; Just in case internet drops
  (handler-case
      (gethash "rate_float"
               (gethash "USD"
                        (gethash "bpi"
                                 (yason:parse
                                  (babel:octets-to-string
                                   (dexador:get *url* :keep-alive t)
                                   :encoding :utf-8)))))
    ;; Return NIL in case some condition is triggered
    (condition () nil)))

;;; Store prices

(defvar *values*
  (make-list (truncate (/ (* 3 60 60) *time-delay*)) ; 3 hours
             :initial-element NIL)
  "List of values got from `*url*'. Set size to a list of the last n
hours getting values: a new coin value is appended in `*values*' every
`*time-delay*', so it is divided the desired n time in seconds by the
time-delay in seconds.")

(defvar *values-average* 0.0
  "Average of values in `*values*'.")

;;; Write on modeline

(defun comma-point (stream arg &rest args)
  (declare (ignore args))
  (format stream
          "~,,',,:D.~A"
          (truncate arg)
          (let ((float-string (format nil "~,2F" arg)))
            (subseq float-string (1+ (position #\. float-string))))))

(defun point-comma (stream arg &rest args)
  (declare (ignore args))
  (format stream
          "~,,'.,:D,~A"
          (truncate arg)
          (let ((float-string (format nil "~,2F" arg)))
            (subseq float-string (1+ (position #\. float-string))))))

(defun space-comma (stream arg &rest args)
  (declare (ignore args))
  (format stream
          "~,,' ,:D,~A"
          (truncate arg)
          (let ((float-string (format nil "~,2F" arg)))
            (subseq float-string (1+ (position #\. float-string))))))

(defun bitcoin-modeline (ml)
  "Get the actual USD-BTC value, store value in list, preserve list size
popping first value, calculate average and set formatting depending on
value vs average. This function is evaluated on every modeline refresh."
  (declare (ignore ml))
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (> (- now *prev-time*) *time-delay*)
      (progn (setf *prev-time* now)
             ;; Add value to values list, pushing to front
             (push (get-value-from-url) *values*)
             ;; Preserve values list size, popping from end
             (setf *values* (nreverse *values*))
             (pop *values*)
             (setf *values* (nreverse *values*))
             ;; Calculate average of values, excluding NIL values
             ;; that could exist because network issues.
             (let ((clean (remove-if-not #'numberp *values*)))
               (setf *values-average* (/ (reduce #'+ clean)
                                         (if (zerop (length clean))
                                             1
                                             (length clean))))))))
  ;; Actual value must be positive number
  (if (and (numberp (car *values*)) (plusp (car *values*)))
      ;; Apply desired format to value
      (let ((value-string
              (case *local-code*
                (0 (format nil "~,2F" (car *values*)))
                (1 (format nil "~/bitcoin::comma-point/" (car *values*)))
                (2 (format nil "~/bitcoin::point-comma/" (car *values*)))
                (3 (format nil "~/bitcoin::space-comma/" (car *values*)))
                (otherwise (format nil "~,2F" (car *values*))))))
        ;; Return with color if desired
        (if *modeline-use-colors*
            (let* ((diff (- (car *values*) *values-average*))
                   (pdiff (/ diff (if (zerop (car *values*))
                                      1
                                      (car *values*)))))
              (cond ((> pdiff *threshold*)
                     (format nil "^[^B^3*~A^]" value-string))
                    ((< pdiff (- *threshold*))
                     (format nil "^[^1*~A^]" value-string))
                    (t (format nil "^[^7*~A^]" value-string))))
            (format nil "^[^**~A^]" value-string)))
      (format nil "-BTC-")))

(stumpwm:add-screen-mode-line-formatter #\b 'bitcoin-modeline)

;;; Debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CL-USER > (declaim (optimize (speed 0) (debug 3) (safety 0)))
;; CL-USER > (asdf:load-system :bitcoin)
;; CL-USER > (in-package "BITCOIN")

;;; Wile executing, swap to code buffers, and any re-compile-load-ed
;;; changes will be visible. Recall `C-c C-b' stops loop in Sly REPL.

;; (do () (nil)
;;   (let* ((price (get-value-from-url))
;;          (clean (remove-if-not #'numberp *values*))
;;          (average (/ (reduce #'+ clean)
;;                      (length clean)))
;;          (diff (- price average)))
;;     (format t "~&~2$ ~2$ ~2@$ ~4@$% ~a"
;;             price
;;             average
;;             diff
;;             (* 100 (/ diff price))
;;             (bitcoin-modeline t)))
;;   (force-output)
;;   (sleep 3))

;; (do () (nil)
;;   (format t "~&~a" (bitcoin-modeline t))
;;   (force-output)
;;   (sleep 1))

;;; Search optimized code to push/append/pop list. See time and conses.

;; (time
;;  (do ((i 1 (1+ i))
;;       (l (make-list 10 :initial-element 0)))
;;      ((> i 10))
;;    (push i l)
;;    (pop l)
;;    (format t "~&~A" l)))

;; (time
;;  (do ((i 1 (1+ i))
;;       (l (make-list 10 :initial-element 0)))
;;      ((> i 1000000) (format t "~&~A" l))
;;    (setf l (append l (list i)))
;;    (pop l)))

;;; Best option and the car is the last pushed element
;; (time
;;  (do ((i 1 (1+ i))
;;       (l (make-list 10 :initial-element 0)))
;;      ((> i 1000000) (format t "~&~A" l))
;;    (push i l)
;;    (setf l (nreverse l))
;;    (pop l)
;;    (setf l (nreverse l))))

;;; Seek for optimal number formatting function
;;; From https://stackoverflow.com/questions/35012859

;; (defun comma-point (stream arg &rest args)
;;   (declare (ignore args))
;;   (format stream
;;           "~,,',,:D.~A"
;;           (truncate arg)
;;           (let ((float-string (format nil "~,2F" arg)))
;;             (subseq float-string (1+ (position #\. float-string))))))

;; (defun point-comma (stream arg &rest args)
;;   (declare (ignore args))
;;   (format stream
;;           "~,,'.,:D,~A"
;;           (truncate arg)
;;           (let ((float-string (format nil "~,2F" arg)))
;;             (subseq float-string (1+ (position #\. float-string))))))

;; (defun space-comma (stream arg &rest args)
;;   (declare (ignore args))
;;   (format stream
;;           "~,,' ,:D,~A"
;;           (truncate arg)
;;           (let ((float-string (format nil "~,2F" arg)))
;;             (subseq float-string (1+ (position #\. float-string))))))

;; (defun custom (stream arg &rest args)
;;   (declare (ignore args))
;;   (multiple-value-bind (quotient remainder) (truncate arg)
;;     (format stream
;;             "~,,'.,:D,~D"
;;             quotient
;;             (truncate (* 100 remainder)))))
