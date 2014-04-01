;;;; swm-emacs.lisp

(in-package #:swm-emacs)

(export '(start-emacs-daemon
          stop-emacs-daemon
          restart-emacs-daemon
          *emacs-start-daemon*
          *emacs-stop-daemon*
          eval-on-daemon))
;; These commands are/were inspired by DSS emacs module in github.com/dss-project/dss-modules/emacs
(defvar *emacs-start-daemon* "emacs --daemon"
  "Command to start the emacs daemon")
(defvar *emacs-stop-daemon* "emacsclient -e \"(save-some-buffers)\" \"(kill-emacs)\" "
  "Command to stop the emacs daemon")
(defvar emacs-module-dir
  #.(asdf:system-relative-pathname (asdf:find-system :swm-emacs)
                                   (make-pathname :directory
                                                  '(:relative "."))))

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
  (run-shell-command *emacs-stop-daemon* t))
(defun restart-daemon-e ()
  (and (stop-daemon-e)
       (start-daemon-e)))

(add-hook *quit-hook* #'stop-daemon-e)

(defcommand start-emacs-daemon () ()
  (if (start-daemon-e)
      (message "Emacs daemon started!")
      (error "Cannot start daemon!")))
(defcommand stop-emacs-daemon () ()
  (if (stop-daemon-e)
      (message "Emacs daemon stopped!")
      (error "Cannot stop daemon!")))
(defcommand restart-emacs-daemon () ()
  (restart-daemon-e))
