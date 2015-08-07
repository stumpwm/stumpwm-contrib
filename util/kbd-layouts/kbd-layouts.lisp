;;;; kbd-layouts.lisp

(in-package #:kbd-layouts)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar *available-keyboard-layouts* nil)

;; Output from 'setxkbmap -print' is authoritative
(defvar *current-keyboard-layout* nil)

;; Available options:
;; :normal -> CapsLock
;; :ctrl   -> Ctrl
(defvar *caps-lock-behavior* nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun keyboard-layout-list (&rest layouts)
  "Return a circular list of keyboard layouts (as given to
   setxkbmap). The first layout in the list will be the default."
  (rplacd (last layouts) layouts)
  (setf *available-keyboard-layouts* layouts)
  ;; Immediately select first layout from the list
  (switch-keyboard-layout))

(defun current-keyboard-layout (ml)
  "Return current keyboard layout"
  (declare (ignore ml))
  (let ((cmd "setxkbmap -print | awk -F'+' '/xkb_symbols/ {print $2}'"))
    (string-trim '(#\Newline) (run-shell-command cmd t))))

;; Make the current keyboard layout accessible from the modeline via %L.
(add-screen-mode-line-formatter #\L #'current-keyboard-layout)

;;;;;;;;;;;;;;
;; Commands ;;
;;;;;;;;;;;;;;

(defcommand switch-keyboard-layout () ()
            (let* ((layout (pop *available-keyboard-layouts*))
                   (cmd (format nil "setxkbmap ~a" layout)))
              
;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

(keyboard-layout-list "us")

(setf *caps-lock-behavior* :normal)

(define-key *top-map* (kbd "S-space") "switch-keyboard-layout")

;; Run it once to set the default layout
;; (switch-keyboard-layout)
