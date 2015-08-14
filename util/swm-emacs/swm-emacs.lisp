;;;; swm-emacs.lisp

(in-package #:swm-emacs)

(export '(emacs-daemon-start
          emacsclient-eval
          emacs-daemon-stop
          emacs-daemon-kill-force
          *emacs-daemon-name*
          *emacsclient-last-load-file*
          *emacsclient-last-command*))

(defvar *emacs-daemon-name* "stumpwm-emacs")
(defvar *emacsclient-last-load-file* nil)
(defvar *emacsclient-last-command* nil)
(defvar emacs-module-dir
  #.(asdf:system-relative-pathname (asdf:find-system :swm-emacs)
                                   (make-pathname :directory
                                                  '(:relative "."))))
(defun emacs-daemon-run-p ()
  "Test emacs daemon run status."
  (string-equal
   (remove #\Newline
           (run-shell-command
            (concat
             "emacsclient "
             "--socket-name=" *emacs-daemon-name*
             " --eval 't'") t)) "t"))

(defcommand emacs-daemon-start () ()
  "Start emacs daemon."
  (if (not (emacs-daemon-run-p))
      (run-shell-command
       (concat "xterm -T emacs-daemon-start -e emacs "
               (if *emacs-daemon-name*
                   (concat "--daemon=" *emacs-daemon-name*)
                 "--daemon")))
    (message (format nil "Emacs daemon (\"~a\") is running ..." *emacs-daemon-name*))))

(defun emacs-generate-elisp-string (expr)
  "Format an Expression to elisp string."
  (cl-ppcre:regex-replace-all
   "swm-emacs:"
   (cl-ppcre:regex-replace-all
    "swm-emacs::"
    (write-to-string expr :case :downcase) "") ""))

(defun emacs-escape-instance-name (str)
  "Generate a valid instance name from `str'."
  (let (buf)
    (map nil #'(lambda (ch)
                 (if (or (char= ch #\()
                         (char= ch #\))
                         (char= ch #\")
                         (char= ch #\ )
                         (char= ch #\Newline))
                     (push #\_ buf)
                     (push ch buf)))
         str)
    (coerce (reverse buf) 'string)))

(defun emacsclient-eval (&optional expression create-frame frame-name-prefix frame-name)
  "Eval an emacs lisp expression with emacsclient"
  (cl-fad:with-open-temporary-file
   (stream :template "/tmp/stumpmw-emacsclient-load-file-%s.el"
           :keep t)
   (let* ((expression-string
           (emacs-generate-elisp-string expression))
          (name (concat
                 (concat (or frame-name-prefix "STUMPWM-EMACS") "-")
                 (or frame-name
                     (emacs-escape-instance-name expression-string))))
          (elisp-file-name (namestring (pathname stream)))
          (emacsclient-command
           (format nil
                   (concat "emacsclient ~@[--socket-name=~A ~]"
                           "~:[ ~;-c~] ~@[-F '((name . \"~A\"))' ~]"
                           "~@[--eval '(load \"~A\")' ~]")
                   *emacs-daemon-name*
                   create-frame
                   name
                   elisp-file-name)))

     ;; Delete last emacsclient-load-file
     (let ((file *emacsclient-last-load-file*))
       (when (and file (probe-file file))
         (delete-file file)))

     ;; write emacs lisp sexp to temp file, all symbol names
     ;; will be converted to downcase when saved.
     (with-standard-io-syntax
      (format stream "~A" expression-string))

     ;; update *emacsclient-last-load-file*
     (setf *emacsclient-last-load-file* elisp-file-name)

     ;; start emacs daemon.
     (if (not (emacs-daemon-run-p))
         (emacs-daemon-start)
       (progn
         (setf *emacsclient-last-command* emacsclient-command)
         (run-or-raise emacsclient-command `(:instance ,name)))))))

;;; TODO: Add a macro `define-emacs-command', which can define emacs command like this:
;;
;; (define-emacs-command swm-gnus (t "GNUS" "swm-gnus") () ()
;;   (gnus)
;;   (message "Start gnus successful!!!"))
;;
;; This macro NEED Help !!!

(defcommand emacs-daemon-stop () ()
  "Stop emacs daemon."
  (emacsclient-eval
   '(progn
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system)
        (when (not x-display-name)
          (setq x-display-name (getenv "DISPLAY")))
        (select-frame (make-frame-on-display
                       x-display-name
                       '((window-system . x)))))
      (let ((last-nonmenu-event nil)
            (window-system "x"))
        (save-buffers-kill-emacs)))
   t))

(defcommand emacs-daemon-kill-force () ()
  "Force stop emacs daemon."
  (run-shell-command
   (format nil "for i in `ps aux | grep emacs | grep =~A | grep -v grep | awk '{print $2}'`; do kill -9 $i; done"
           *emacs-daemon-name*))
  (message (format nil "Emacs daemon which name is (~A) has been killed" *emacs-daemon-name*)))

(defcommand swm-emacs () ()
  "Start emacs"
  (emacsclient-eval
   '(message "Start emacs from stumpwm...")
   t))

(add-hook *quit-hook* #'emacs-daemon-stop)
