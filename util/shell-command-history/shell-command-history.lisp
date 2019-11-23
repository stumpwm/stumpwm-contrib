;;;; command-history.lisp

(in-package #:shell-command-history)

(defvar *home-dir* (getenv "HOME"))

(defvar *shell-command-history-file*  (merge-pathnames (format nil
                                                               "~A/.stumpwm.d/shell-history"
                                                               *home-dir*)))

(defun load-input-history ()
  "Load *input-history* to file."
  (with-open-file (in *shell-command-history-file* :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (setf stumpwm::*input-shell-history* (read in))))))

(defun save-input-history ()
  "Save current *input-history* to file."
  (with-open-file (out *shell-command-history-file*
                       :direction         :output
                       :if-does-not-exist :create
                       :if-exists         :supersede)
    (with-standard-io-syntax
      (print (remove-duplicates stumpwm::*input-shell-history* :test #'string= :from-end t)
             out))))

(add-hook *start-hook* 'load-input-history)

(add-hook *quit-hook* 'save-input-history)
