(in-package #:clipboard-history)

(defmacro push-max-stack (stack val max-depth)
  `(setq ,stack
         (cons ,val
               (if (<= ,max-depth (length ,stack))
                   (subseq ,stack 0 (1- ,max-depth))
                   ,stack))))

(defun string-maxlen (s maxlen)
  (let ((s1 (subseq s 0 (min maxlen (length s)))))
    (if (string-equal s1 s)
        s1
        (stumpwm:concat s1 " ..."))))

(defun poll-selection (&optional (selection :primary))
  (xlib:convert-selection selection
                          :utf8_string
                          (stumpwm::screen-input-window
                            (stumpwm:current-screen))
                          :stumpwm-selection))

;; (poll-selection)

(defun poll-clipboard-selection ()
  (poll-selection :clipboard))

(defun basic-get-x-selection (&optional (selection :clipboard))
  (getf stumpwm:*x-selection* selection))

;; (basic-get-x-selection)

(defvar *clipboard-history* nil)
(defparameter *clipboard-history-max-length* 32)

(defun save-clipboard-history (sel)
  (when (and (stringp sel)
             (not (zerop (length sel)))
             (not (member sel *clipboard-history* :test 'string-equal)))
    (push-max-stack *clipboard-history* sel *clipboard-history-max-length*)))

(stumpwm:add-hook stumpwm:*selection-notify-hook* 'clipboard-history::save-clipboard-history)

(stumpwm:defcommand show-clipboard-history () ()
  "Select from previously saved selections"
  (if (null *clipboard-history*)
      (stumpwm:message "No selection history")
      (let ((sel (second
                  (stumpwm:select-from-menu
                   (stumpwm:current-screen)
                   (mapcar (lambda (s)
                             (list (stumpwm::escape-caret (string-maxlen s 32)) s))
                           *clipboard-history*)
                   nil))))
        (when sel
          (stumpwm:set-x-selection sel :clipboard)))))

(defvar *clipboard-timer* nil)

(defun stop-clipboard-manager ()
  (when (stumpwm:timer-p *clipboard-timer*)
    (stumpwm:cancel-timer *clipboard-timer*)
    (setq *clipboard-timer* nil)))

;; (stop-clipboard-manager)

(defvar *clipboard-poll-timeout* 5)

(defun start-clipboard-manager ()
  (stop-clipboard-manager)
  (setf *clipboard-timer*
        (stumpwm:run-with-timer (- *clipboard-poll-timeout*
                                   (mod (get-decoded-time) *clipboard-poll-timeout*))
                                *clipboard-poll-timeout*
                                'poll-clipboard-selection)))

;; (start-clipboard-manager)

(stumpwm:defcommand clear-clipboard-history () ()
  "Clear saved selections"
  (setf *clipboard-history* nil))
