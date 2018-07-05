(in-package #:remap-keys)

(defvar *remap-keys-window-class-list* nil)

(defun find-remap-keys-window-class (class)
  (first
   (member-if (lambda (pattern)
                (stumpwm::string-match class pattern))
              *remap-keys-window-class-list*
              :key 'car)))

(stumpwm:defcommand send-remapped-key () ()
  "If the WINDOW-CLASS of the current window matches a previously
defined REMAP-KEYS rule (see REMAP-KEYS:DEFINE-REMAPPED-KEYS), this
command looks up the most recently triggered key sequence in that rule
and forwards the new key-sequence to the target window."
  (let* ((raw-key (first stumpwm::*current-key-seq*))
         (keymap (cdr
                (find-remap-keys-window-class
                 (stumpwm:window-class (stumpwm:current-window)))))
         (keys (cdr
                (assoc (stumpwm::print-key raw-key) keymap :test 'equal))))
    (stumpwm:dformat 1 "~s ~s ~s~%"
                     (stumpwm:window-class (stumpwm:current-window))
                     (stumpwm::print-key raw-key)
                     (when keys
                       (mapcar 'stumpwm::print-key keys)))
    (if keys
        (dolist (key keys)
          (stumpwm::send-meta-key (stumpwm:current-screen) key))
        (stumpwm::send-meta-key (stumpwm:current-screen) raw-key))))

(defun make-remap-keys (kmap)
  (mapcar (lambda (k)
            (let ((kcodes (mapcar (lambda (key)
                                    (or (stumpwm:kbd key)
                                        (throw 'error
                                          (format nil "Invalid keyspec: ~S" key))))
                                  (if (consp (cdr k))
                                      (cdr k)
                                      (list (cdr k))))))
              (cons (car k) kcodes)))
          kmap))

(defun define-remapped-keys (specs)
  "Define the keys to be remapped and their mappings. The SPECS
argument needs to be of the following structure:

  (regexp . ((\"key-to-remap\" . <new-keycodes>) ...))

EXAMPLE:
  (remap-keys:define-remapped-keys
    '((\"Firefox\"
       (\"C-n\"   . \"Down\")
       (\"C-p\"   . \"Up\")
       (\"C-k\"   . (\"C-S-End\" \"C-x\")))))

  The above form remaps Ctrl-n to Down arrow, and Ctrl-p to Up arrow
  keys.  The Ctrl-k key is remapped to the sequence of keys
  Ctrl-Shift-End followed by Ctrl-x.
"
  (setq *remap-keys-window-class-list*
        (mapcar (lambda (spec)
                  (let ((pattern (car spec))
                        (kmap (cdr spec)))
                    (cons pattern (make-remap-keys kmap))))
                specs))
  (let ((keys (mapcar 'car
                      (mapcan 'cdr *remap-keys-window-class-list*))))
    (dolist (k keys)
      (stumpwm:define-key stumpwm:*top-map*
          (stumpwm:kbd k)
        "send-remapped-key"))))

(stumpwm:defcommand send-raw-key () ()
  "Prompts for a key and forwards it to the CURRENT-WINDOW."
  (let* ((k (stumpwm::read-key))
         (code (car k))
         (state (cdr k))
         (screen (stumpwm:current-screen)))
    (when (stumpwm:screen-current-window screen)
      (let ((win (stumpwm:screen-current-window screen)))
        (xlib:send-event (stumpwm:window-xwin win)
                         :key-press (xlib:make-event-mask :key-press)
                         :display stumpwm:*display*
                         :root (stumpwm:screen-root
                                (stumpwm:window-screen win))
                         ;; Apparently we need these in here, though they
                         ;; make no sense for a key event.
                         :x 0 :y 0 :root-x 0 :root-y 0
                         :window (stumpwm:window-xwin win)
                         :event-window (stumpwm:window-xwin win)
                         :code code
                         :state state)))))
