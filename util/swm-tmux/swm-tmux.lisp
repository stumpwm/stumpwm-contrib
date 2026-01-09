;;;; swm-tmux.lisp

(in-package #:swm-tmux)

(defvar *swm-tmux-default-term* "xterm"
  "Terminal emulator to use for tmux sessions.")

(defvar *swm-tmux-default-term-title-opt* "-T"
  "Command line option for setting terminal title. Set to NIL if your
terminal does not support title option.")

(defvar *swm-tmux-default-term-exec-opt* "-e"
  "Command line option for executing a command in the terminal.")

(defun tmux-sessions ()
  "Return a list of existing tmux session names."
  (let ((output (run-shell-command "tmux ls -F '#{session_name}' 2>/dev/null" t)))
    (when (and output (not (string= output "")))
      (cl-ppcre:split "\\n" (string-trim '(#\Newline) output)))))

(defun next-session-name ()
  "Generate next session name based on current group name.
If group name is not already a session, use it directly.
Otherwise append -N where N is the next available number."
  (let* ((prefix (group-name (current-group)))
         (sessions (tmux-sessions)))
    (if (not (member prefix sessions :test #'string=))
        prefix
        (let* ((pattern (format nil "^~a-(\\d+)$" prefix))
               (nums (loop for s in sessions
                           for match = (cl-ppcre:scan-to-strings pattern s)
                           when match
                             collect (parse-integer
                                      (aref (nth-value 1 (cl-ppcre:scan-to-strings pattern s)) 0)))))
          (format nil "~a-~a" prefix (1+ (or (reduce #'max nums :initial-value 0) 0)))))))

(defun build-term-command (session-name tmux-cmd)
  "Build terminal command string for running tmux.
SESSION-NAME is used for window title, TMUX-CMD is the tmux command to run."
  (let ((title (format nil "tmux/~a" session-name)))
    (if *swm-tmux-default-term-title-opt*
        (format nil "~a ~a '~a' ~a ~a"
                *swm-tmux-default-term*
                *swm-tmux-default-term-title-opt*
                title
                *swm-tmux-default-term-exec-opt*
                tmux-cmd)
        (format nil "~a ~a ~a"
                *swm-tmux-default-term*
                *swm-tmux-default-term-exec-opt*
                tmux-cmd))))

(stumpwm:defcommand swm-tmux-session () ()
  "Select an existing tmux session to attach or create a new one.
Presents a menu with existing sessions and a [New session] option.
Session names default to current group name."
  (let* ((sessions (tmux-sessions))
         (options (cons '("[New session]") (mapcar #'list sessions)))
         (choice (select-from-menu (current-screen) options "Tmux session: ")))
    (when choice
      (if (string= (first choice) "[New session]")
          ;; New session: prompt for name
          (let ((session-name (read-one-line (current-screen)
                                             "Session name: "
                                             :initial-input (next-session-name))))
            (when (and session-name (not (string= session-name "")))
              (let ((tmux-cmd (if (member session-name sessions :test #'string=)
                                  (format nil "tmux attach -t '~a'" session-name)
                                  (format nil "tmux new-session -s '~a'" session-name))))
                (run-shell-command (build-term-command session-name tmux-cmd)))))
          ;; Existing session: attach directly
          (let* ((session-name (first choice))
                 (tmux-cmd (format nil "tmux attach -t '~a'" session-name)))
            (run-shell-command (build-term-command session-name tmux-cmd)))))))

(stumpwm:defcommand swm-tmux-new-session () ()
  "Create a new tmux session with auto-generated name based on current group."
  (let* ((session-name (next-session-name))
         (tmux-cmd (format nil "tmux new-session -s '~a'" session-name)))
    (run-shell-command (build-term-command session-name tmux-cmd))))
