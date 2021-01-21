(in-package #:stump-lock)

(defvar *password* nil
  "The password to unlock.")

(stumpwm:defcommand lock-screen () ()
  (unless *password*
    (error "A password needs to be set. Please read the instructions."))
  (loop
    (if (string= (let ((stumpwm::*input-history* nil))
                   (stumpwm:read-one-line
                    (stumpwm:current-screen)
                    (format nil "The screen is locked.~%Enter password: ")
                    :password t))
                 *password*)
        (return)
        (sleep (+ 2 (random 3.0))))))
