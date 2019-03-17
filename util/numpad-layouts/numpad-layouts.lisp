;;;; numpad-layouts.lisp

(in-package #:numpad-layouts)
(export '(set-numpad-layout))


(defparameter *layouts*
  '((us . ((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
           (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
           (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17)
           (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 17)))
    (fr . ((87 10 . 17) (88  11 . 17) (89 12 . 17) (106 60 . 17)
           (83 13 . 17) (84  14 . 17) (85 15 . 17) (86  21 . 17)
           (79 16 . 17) (80  17 . 17) (81 18 . 17) (63  51 . 16)
           (82 15 . 16) (104 36 . 16) (91 59 . 17) (90  19 . 17)))
    (se . ((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
           (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
           (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17)
           (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 17))))
  "An alist of numpad layouts for non-standard keyboards that move the
  locations of 0-9 and the symbols on a numpad (typically -,*,+ and =).")

(defun set-numpad-layout (layout-name)
  "Sets the `*numpad-map*' that StumpWM uses to map numpad keys to
the main keyboard when the NumLock key is active."
  (if (symbolp layout-name)
      (let ((layout (rest (assoc layout-name *layouts* :test #'string=))))
        (if layout
            (setf stumpwm:*numpad-map* layout)
            (error "~a is not a supported layout" layout-name)))
      (error "~a is not of type symbol" layout-name)))
