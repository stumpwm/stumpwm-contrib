(in-package #:stumpwm-mixer)

(defun mixer-output ()
  (let ((output (run-shell-command "mixer" t)))
    (loop for line in (uiop:split-string output :separator '(#\newline))
          collect (uiop:split-string line :separator '(#\space)))))

(defun parse-mixer ()            
  (let ((output (mixer-output))
        (rem '("" "Mixer" "is" "currently" "set" "to")))
    (loop for line in output
          collect (remove-if
                   (lambda (x)
                     (member x rem :test #'equalp))
                   line))))

(defun print-audio ()
  (let* ((output (parse-mixer))
         (volume (car (cdr (assoc "pcm" output :test #'equalp))))
         (muted (car (cdr (assoc "vol" output :test #'equalp)))))
    (message 
     (format nil "Volume: ~a~:[~; (muted)~]" volume
             (string= "0:0" muted)))))

(defcommand volume-up () ()
  (run-shell-command
   "mixer pcm +5")
  (print-audio))

(defcommand volume-down () ()
  (run-shell-command
   "mixer pcm -5")
  (print-audio))

(defcommand toggle-mute () ()
  (let ((vol (assoc "vol" (parse-mixer)
                    :test #'equalp)))
    (if (equalp (cadr vol) "0:0")
        (run-shell-command
         "mixer vol 100")
        (run-shell-command
         "mixer vol 0"))
    (print-audio)))

(defcommand set-mute () ()
  (run-shell-command
   "mixer vol 0"))

(defcommand unset-mute () ()
  (run-shell-command
   "mixer vol 100"))
