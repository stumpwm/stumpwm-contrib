(in-package :stumpwm)

;;; We want to redisplay our clim mode line every time a command is run. 

(defun eval-command (cmd &optional interactivep)
  "exec cmd and echo the result."
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input
                                                  :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
                 (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
        ;; this fancy footwork lets us grab the backtrace from where the
        ;; error actually happened.
        (restart-case
            (handler-bind
                ((error (lambda (c)
                          (invoke-restart 'eval-command-error
                                          (format nil "^B^1*Error In Command '^b~a^B': ^n~A~a"
                                                  cmd c (if *show-command-backtrace*
                                                            (backtrace-string) ""))))))
              (parse-and-run-command cmd))
          (eval-command-error (err-text)
            :interactive (lambda () nil)
            (values err-text t)))
      ;; interactive commands update the modeline
      (update-all-mode-lines)
      (clim-mode-line::redisplay-clim-mode-line)
      (cond ((stringp result)
             (if error-p
                 (message-no-timeout "~a" result)
                 (message "~a" result)))
            ((eq result :abort)
             (unless *suppress-abort-messages*
               (message "Abort.")))))))
