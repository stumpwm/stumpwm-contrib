(in-package #:clim-mode-line)

(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

(defvar *align-x* :left)

(defvar *highlight-drop* nil)

(defvar *display-as-table* nil)

(defvar *display-style* :text
  "Should be one of either :table or :text.")

(defvar *text-display-formatter-intermix* " "
  "Spacing between each formatters output when in text mode. This string is 
formatted to the output stream in between each formatter when in text mode.")

(defmacro cell ((pane) &body body)
  (declare (ignore pane))
  `(formatting-cell (slim:*pane*
                     ;; ,pane
                     ;; :align-x *align-x*
                     )
     ,@body))

(defun call-next-formatter (formatter-list frame pane)
  (when (eql *display-style* :text)
    (format pane "~A" *text-display-formatter-intermix*))
  (when formatter-list
    (funcall (car formatter-list) frame pane (cdr formatter-list))))

(defmacro define-delimiter (name string)
  `(defun ,(intern (format nil "FORMAT-~A" name)) (frame pane other-formatters)
     (if *display-as-table* 
         (cell (pane) (format pane ,string))
         (format pane ,string))
     (call-next-formatter other-formatters frame pane)))

(define-delimiter space " ")
(define-delimiter bar "|")
(define-delimiter left-bracket "[")
(define-delimiter right-bracket "]")

(defun format-align-right (frame pane other-formatters)
  (with-right-alignment (frame pane)
    (call-next-formatter other-formatters frame pane))
  ;; (let ((*align-x* :right))
  ;;   (call-next-formatter other-formatters frame pane))
  )

(defun invoke-cell-with-stumpwm-highlighting (pane highlight-thunk continuation
                                              &key (dropped *highlight-drop*))
  (if (funcall highlight-thunk)
      (if dropped
          (cell (pane)
            (with-inverted-ink (pane)
              (funcall continuation pane)))
          (with-inverted-ink (pane)
            (cell (pane)
              (funcall continuation pane))))
      (cell (pane)
        (funcall continuation pane))))

(defmacro with-stumpwm-cell-highlighting ((pane highlight-when &key dropped)
                                          &body body)
  (alexandria:with-gensyms (continuation highlight-thunk contarg)
    `(flet ((,continuation (,contarg)
              (declare (ignore ,contarg))
              ,@body)
            (,highlight-thunk ()
              ,highlight-when))
       (declare (dynamic-extent (function ,continuation)
                                (function ,highlight-thunk)))
       (invoke-cell-with-stumpwm-highlighting ,pane
                                              #',highlight-thunk
                                              #',continuation
                                              ,@(when dropped 
                                                  `(:dropped ,dropped))))))

(defun invoke-with-normal-stumpwm-highlighting (pane highlight-thunk continuation)
  "When HIGHLIGHT-THUNK is returns true, invoke CONTINUATION with the StumpWM
manner of highlighting, by swapping the foreground and the background. If this is 
called within a table, invoke CONTINUATION within a cell."
  (flet ((continue-processing ()
           (with-cell (pane)
             (funcall continuation pane))))
    (declare (dynamic-extent (function continue-processing)))
    (if (funcall highlight-thunk)
        (with-inverted-ink (pane)
          (continue-processing))
        (continue-processing))))

(defmacro with-normal-stumpwm-highlighting ((pane highlight-when)
                                            &body body)
  (alexandria:with-gensyms (continuation highlight-thunk contarg)
    `(flet ((,continuation (,contarg)
              (declare (ignore ,contarg))
              ,@body)
            (,highlight-thunk ()
              ,highlight-when))
       (declare (dynamic-extent (function ,continuation)
                                (function ,highlight-thunk)))
       (invoke-with-normal-stumpwm-highlighting ,pane
                                                #',highlight-thunk
                                                #',continuation))))

(defun invoke-with-thin-stumpwm-highlighting (pane highlight cont)
  (flet ((continue-processing ()
           (with-cell (pane)
             (funcall cont))))
    (declare (dynamic-extent (function continue-processing)))
    (if (funcall highlight)
        (let ((record (with-undrawn-output-record (pane)
                        (with-drawing-options (pane :ink +background-ink+)
                          (continue-processing)))))
          (with-output-record-bounds (x y w h) record
            (draw-rectangle* pane x y (+ x w) (+ y h))
            (replay record pane)))
        (continue-processing))))

(defmacro with-thin-stumpwm-highlighting ((pane highlight-when) &body body)
  (alexandria:with-gensyms (continuation highlight)
    `(flet ((,continuation ()
              ,@body)
            (,highlight ()
              ,highlight-when))
       (declare (dynamic-extent (function ,continuation)
                                (function ,highlight)))
       (invoke-with-thin-stumpwm-highlighting ,pane
                                              #',highlight
                                              #',continuation))))

(defun invoke-with-stumpwm-highlighting (pane highlight cont style)
  (case style
    ((:normal :thick)
     (invoke-with-normal-stumpwm-highlighting pane highlight cont))
    ((:thin)
     (invoke-with-thin-stumpwm-highlighting pane highlight cont))))

(defmacro with-stumpwm-highlighting ((pane highlight-when
                                      &optional (style :normal))
                                     &body body)
  (alexandria:with-gensyms (continuation highlight-thunk contarg)
    `(flet ((,continuation (&rest ,contarg)
              (declare (ignore ,contarg))
              ,@body)
            (,highlight-thunk ()
              ,highlight-when))
       (declare (dynamic-extent (function ,continuation)
                                (function ,highlight-thunk)))
       (invoke-with-stumpwm-highlighting ,pane
                                         #',highlight-thunk
                                         #',continuation
                                         ,style))))

(defun invoke-with-formatting (pane continuation)
  (with-cell (pane)
    (funcall continuation)))

(defmacro with-formatting ((pane) &body body)
  (alexandria:with-gensyms (cont)
    `(flet ((,cont ()
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-formatting ,pane #',cont))))


(defun format-groups (frame pane other-formatters)
  (declare (ignorable frame))
  (let* ((current-group (stumpwm:current-group))
         (groups (stumpwm::sort-groups (stumpwm:current-screen)))
         (g1 (car groups)))
    (with-stumpwm-highlighting (pane (eq current-group g1))
      (present g1 'stumpwm::group :stream pane :single-box t))
    (dolist (group (cdr groups))
      (format pane " ")
      (with-stumpwm-highlighting (pane (eq current-group group))
        (present group 'stumpwm::group :stream pane :single-box t))))
  (call-next-formatter other-formatters frame pane))

(defun format-windows (frame pane other-formatters)
  (let* ((current-window (stumpwm:current-window))
         (windows (stumpwm::sort-windows-by-number
                   (stumpwm:group-windows (stumpwm:current-group))))
         (w1 (car windows)))
    (when windows
      (with-normal-stumpwm-highlighting (pane (eq current-window w1))
        (present w1 'stumpwm::window :stream pane :single-box t))
      (dolist (win (cdr windows))
        (format pane " ")
        (with-normal-stumpwm-highlighting (pane (eq current-window win))
          (present win 'stumpwm::window :stream pane :single-box t)))))
  (call-next-formatter other-formatters frame pane))

;; (defun format-windows-tableless (frame pane other-formatters)
;;   (with-right-alignment (frame pane)
;;     (dolist (win (stumpwm::sort-windows-by-number
;;                   (stumpwm:group-windows (stumpwm:current-group))))
;;       (with-stumpwm-highlighting (pane (eq current-window win))
;;         (present win 'stumpwm::window :stream pane :single-box t)))
;;     (call-next-formatter other-formatters frame pane)))

