;;;; emacs.lisp

(in-package #:emacs)

(export '(start-daemon
          stop-daemon
          restart-daemon
          *emacs-start-daemon*
          *emacs-stop-daemon*
          eval-on-daemon))
;; These commands are/were inspired by DSS emacs module in github.com/dss-project/dss-modules/emacs
(defvar *emacs-start-daemon* "emacs --daemon"
  "Command to start the emacs daemon")
(defvar *emacs-stop-daemon* "emacsclient -e \"(save-some-buffers)\" \"(kill-emacs)\" "
  "Command to stop the emacs daemon")
(defun eval-on-daemon (expr)
  (run-shell-command (concatenate 'string "emacsclient --eval " 
                                  (string-downcase 
                                   (prin1-to-string expr))) t))
(defun eval-and-raise-daemon (expr)
  (run-shell-command (concatenate 'string "emacsclient -c --eval " 
                                  (string-downcase 
                                   (prin1-to-string expr)))))

(defun start-daemon-e ()
  ;;TODO load utils for interactive quitting of emacs
  (run-shell-command *emacs-start-daemon* t))
(defun stop-daemon-e ()
  (run-shell-command *emacs-stop-daemon*))
(defun restart-daemon-e ()
  (and (stop-daemon-e)
       (start-daemon-e)))

(add-hook *quit-hook* #'stop-daemon-e)

(defcommand start-daemon () ()
  (if (start-daemon-e)
      (message "Emacs daemon started!")
      (error "Cannot start daemon!")))
(defcommand stop-daemon () ()
  (if (stop-daemon-e)
      (message "Emacs daemon stopped!")
      (error "Cannot stop daemon!")))
(defcommand restart-daemon () ()
  (restart-daemon-e))
