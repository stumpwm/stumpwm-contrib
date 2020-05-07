;;;; kbd-layouts.lisp

(in-package #:kbd-layouts)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar *available-keyboard-layouts* '#1=("us" . #1#))

;; Available options:
;; :normal      -> CapsLock
;; :ctrl        -> Ctrl
;; :swapped     -> Swap Ctrl and CapsLock
;; :leave-alone -> do not change CapsLock
(defvar *caps-lock-behavior* :normal)

;; Custom option string appended to setxkbmap
(defvar *custom-setxkb-options* nil)

;; Run xmodmap ~/.Xmodmap each time layouts are switched
(defvar *run-xmodmap* t)

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
  "Perform the actual layout switching."
  (let* ((layout (pop *available-keyboard-layouts*))
         (caps (ecase
                   *caps-lock-behavior*
                 (:normal "caps:capslock")
                 (:ctrl "ctrl:nocaps")
                 (:swapped "ctrl:swapcaps")
                 (:leave-alone nil)))
         (cmd (format nil "setxkbmap ~a~@[ -option ~a~]~@[ ~a~]" layout caps *custom-setxkb-options*)))
    (run-shell-command cmd nil)
    (when *run-xmodmap*
      (run-shell-command "xmodmap ~/.Xmodmap"))
    (message (format nil "Keyboard layout switched to: ~a" layout))))

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

(define-key *top-map* (kbd "s-SPC") "switch-keyboard-layout")
