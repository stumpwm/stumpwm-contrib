;;;; numpad-layouts.lisp

(in-package #:numpad-layouts)
(export '(set-numpad-layout))

(defvar *layouts* '((us . ((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
			   (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
			   (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17) 
			   (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 17)))
		    (se . ((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
			   (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
			   (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17) 
			   (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 17))))
  "An alist of numpad layouts for non-standard keyboards that move the
  locations of 0-9 and the symbols on a numpad (typically -,*,+ and
  =). To define a new layout, copy a current layout and modify the
  keys that are non standard. The first element of the alist is the
  numpad keycode, the second is a dotted pair of the keycode and
  modifier state that the numpad key should represent. In most cases,
  this will be 16 (just numlock pressed), however if you need to
  represent a shift+key, then use 17. If you are unsure of the
  keycode, use the console program `xev' and press the keys in order
  to get the appropriate code.")

(defun set-numpad-layout (layout-name)
  "Sets the `*numpad-map*' that StumpWM uses to map numpad keys to
the main keyboard when the NumLock key is active."
  (if (symbolp layout-name)
      (let (layout (rest (assoc layout-name *layouts*)))
	(if layout
	    (setf stumpwm:*numpad-map* layout)
	    (error "~a is not a supported layout" layout-name)))
      (error "~a is not of type symbol" layout-name)))

