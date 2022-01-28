(in-package #:clim-mode-line)



;; (define-presentation-method present
;;     (group (type stumpwm::group) stream view &key)
;;   (let ((str (string-trim '(#\space)
;;                           (stumpwm::format-expand stumpwm::*group-formatters*
;;                                                   stumpwm::*group-format*
;;                                                   group))))
;;     ;; (with-output-recording-options (stream :record t :draw nil)
;;     ;;   (format stream "~A" str))
;;     (with-new-output-record (stream 'clim:standard-sequence-output-record record)
;;       ;; (with-output-recording-options (stream :record record :draw nil)
;;       ;;   (format stream "~A" str)
;;       ;;   )
;;       (multiple-value-bind (x y)
;;           (cursor-position (stream-text-cursor stream))
;;         (with-sheet-medium (medium stream)
;;           (let* ((text-style (medium-text-style medium)))
;;             (multiple-value-bind (tw th final-x final-y baseline)
;;                 (text-size medium str :text-style text-style)
;;               ;; (declare (ignore tw th baseline))
;;               (draw-rectangle* stream x (+ y 1)
;;                                (+ x tw)
;;                                (+ y th)
;;                                ;; final-x final-y
;;                                :filled t
;;                                :ink +flipping-ink+ ;; +black+ ;; +foreground-ink+
;;                                                           )
;;               (setf (cursor-position (stream-text-cursor stream)) (values x y))
;;               (with-drawing-options (stream :ink ;; +red+
;;                                             +flipping-ink+
;;                                             )
;;                 (format stream "~A" str)))))
        
        
;;         ;; (draw-design (sheet-medium stream)
;;         ;;              (bounding-rectangle record)
;;         ;;              :ink +black+ ;; +flipping-ink+
;;         ;;              :filled t)
;;         )
;;       ;; (with-drawing-options (stream :ink +flipping-ink+)
;;       ;;   (format stream "~A" str))
;;       )
;;     ;; (draw-design (sheet-medium stream)
;;     ;;              )
;;     ;; (format stream "~A" str)
;;     ))

(define-presentation-method present
    (group (type stumpwm::group) stream view &key)
  (let ((str (string-trim '(#\space)
                          (stumpwm::format-expand stumpwm::*group-formatters*
                                                  stumpwm::*group-format*
                                                  group))))
    (format stream "~A" str)))

(define-presentation-method present
    (window (type stumpwm::window) stream view &key)
  (let ((str (string-trim '(#\space)
                          (stumpwm::format-expand stumpwm::*window-formatters*
                                                  stumpwm::*window-format*
                                                  window))))
    (format stream "~A" str)))
