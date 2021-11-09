;;;; binwarp.lisp

(in-package #:binwarp)

(defvar *binwarp-mode-p*     nil "Whether you are in the binwarp mode.")
(defvar *binwarp-area*       nil "The current screen area that binwarp is restricted by.")
(defvar *binwarp-history*    '() "The list of previous binwarp areas.")
(defvar *reinitiate-ptr*     nil "If non-nil, put pointer in *INIT-PTR-POSITION* upon BINWARP-MODE activation.")
(defvar *init-ptr-position*  nil "Two-element list of PTR-X and PTR-Y to put the pointer at when binwarp-mode starts.")
(defvar *preserve-history*   nil "Whether to keep binwarping history between sessions if non-nil.")

(defclass binwarp-area ()
  ((x      :initarg :x      :type 'integer :accessor x)
   (y      :initarg :y      :type 'integer :accessor y)
   (height :initarg :height :type 'integer :accessor height)
   (width  :initarg :width  :type 'integer :accessor width)
   (ptr-x                   :type 'integer :accessor ptr-x)
   (ptr-y                   :type 'integer :accessor ptr-y)))

(defun copy-binwarp-area (bw-area &key x y height width)
  (make-instance 'binwarp-area
                 :y (or y (y bw-area))
                 :x (or x (x bw-area))
                 :height (or height (height bw-area))
                 :width (or width (width bw-area))))

(defmacro with-pointer ((x-var y-var) &body body)
  "Convenience macro to get the pointer position and bind it to the given variables,
x coordinate to X-VAR and y coordinate to Y-VAR respectively."
  `(multiple-value-bind (,x-var ,y-var)
       (xlib:global-pointer-position *display*)
     ,@body))

(defcommand randwarp (direction &optional (maximum-step 10) (base-step 0)) (:direction :number :number)
  "Ratwarps the pointer in the direction of DIRECTION to BASE-STEP pixels plus
random number of pixels bounded by MAXIMUM-STEP"
  (case direction
    (:rigth (ratrelwarp (+ base-step (random maximum-step)) 0))
    (:left  (ratrelwarp (+ (- base-step) (- (random maximum-step))) 0))
    (:up    (ratrelwarp 0 (+ (- base-step) (- (random maximum-step)))))
    (:down  (ratrelwarp 0 (+ base-step (random maximum-step))))
    (t nil)))

(defun binwarp-to-area (area)
  "Warp the mouse to the mouse position stored in the given binwarp-AREA."
  (ratwarp (ptr-x area) (ptr-y area)))

(defun centered-warp (area)
  "Ratwarp to the center on the binwarp-AREA."
  (ratwarp (round (+ (x area) (/ (width  area) 2)))
           (round (+ (y area) (/ (height area) 2)))))

(defcommand init-binwarp () ()
  "Default initialization function for Binwarp mode."
  (let* ((screen (current-screen))
         (x 0)
         (y (if (and (stumpwm::head-mode-line (current-head))
                     (eq *mode-line-position* :top))
                (stumpwm::mode-line-height
                 (stumpwm::head-mode-line (current-head)))
                0))
         (h (screen-height screen))
         (w (screen-width screen)))
    (setf *binwarp-area* (make-instance 'binwarp-area :x x :y y :height h :width w)
          *binwarp-mode-p* t))
  (when *reinitiate-ptr*
    (apply #'ratwarp *init-ptr-position*)))

(defcommand exit-binwarp () ()
  "Default wrapping-up function for Binwarp mode."
  (setf *binwarp-mode-p* nil
        *binwarp-history* (when *preserve-history*
                            *binwarp-history*)))

(defcommand back-binwarp () ()
  "Return to the previous binwarp area and mouse position.
Be careful, the history movement is not tree-like and is destructive!

You won't be able to go back after you've gone back, I mean :)"
  (when *binwarp-history*
    (binwarp-to-area (setf *binwarp-area* (pop *binwarp-history*)))))

(defcommand binwarp (direction) (:direction)
  "Splits the current *BINWARP-AREA* in two over the pointer position and
moves the pointer to the center of this area -- in the direction of the GRAVITY."
  (with-pointer (pointer-x pointer-y)
    (setf (ptr-x *binwarp-area*) pointer-x
          (ptr-y *binwarp-area*) pointer-y)
    (push *binwarp-area* *binwarp-history*)
    (setf *binwarp-area* (copy-binwarp-area
                          *binwarp-area*
                          :height (when (member direction '(:up :down))
                                    (/ (height *binwarp-area*) 2))
                          :y (when (eq direction :down)
                               (+ (y *binwarp-area*)
                                  (/ (height *binwarp-area*) 2)))
                          :x (when (eq direction :right)
                               (+ (x *binwarp-area*)
                                   (/ (width *binwarp-area*) 2)))
                          :width (when (member direction '(:left :right))
                                   (/ (width *binwarp-area*) 2))))
    (centered-warp *binwarp-area*)))

(defvar *default-binwarp-keymap*
  '(((stumpwm:kbd "Down") "binwarp down")
    ((stumpwm:kbd "n") "binwarp down")
    ((stumpwm:kbd "j") "binwarp down")

    ((stumpwm:kbd "Up") "binwarp up")
    ((stumpwm:kbd "p") "binwarp up")
    ((stumpwm:kbd "k") "binwarp up")

    ((stumpwm:kbd "Right") "binwarp right")
    ((stumpwm:kbd "f") "binwarp right")
    ((stumpwm:kbd "l") "binwarp right")

    ((stumpwm:kbd "Left") "binwarp left")
    ((stumpwm:kbd "b") "binwarp left")
    ((stumpwm:kbd "h") "binwarp left")

    ((stumpwm:kbd "0") "init-binwarp")

    ((stumpwm:kbd "C-z") "back-binwarp")
    ((stumpwm:kbd "/") "back-binwarp")
    ((stumpwm:kbd "u") "back-binwarp"))
  "The default keymap for binwarping that tries to be comfortable for
Emacs, CUA and Vi(m) users all at once.")

(defmacro define-binwarp-mode (name key
                               (&key
                                  (map '*top-map*)
                                  (exit-keys '((kbd "C-g") (kbd "ESC")))
                                  (on-enter '#'binwarp:init-binwarp)
                                  (on-exit  '#'binwarp:exit-binwarp)
                                  (redefine-bindings nil))
                               &body bindings)
  "Macro you'd most probably use in your config.

NAME has the same constraints as the name of for the `define-interactive-keymap'.
KEY should be a string to be passed to `kbd'.
MAP should be a keymap. It's `*top-map*' by default.
ON-ENTER, ON-EXIT are the same as is `define-interactive-keymap'.
EXIT-KEYS are the same as `:exit-on' of `define-interactive-keymap'.
REDEFINE-BINDINGS defines whether the BINDINGS will be appended
to the `*default-binwarp-keymap*' if it's nil, and replace
the default keymap otherwise.

And BINDINGS follow the lambda list as an implicit list.

Examples:

\(binwarp:define-binwarp-mode binwarp-mode \"s-m\"
  ((kbd \"C-n\") \"ratrelwarp  0 +5\")
  ((kbd \"C-p\") \"ratrelwarp  0 -5\")
  ((kbd \"C-f\") \"ratrelwarp +5  0\")
  ((kbd \"C-b\") \"ratrelwarp -5  0\")

  ((kbd \"RET\") \"ratclick 1\")
  ((kbd \"SPC\") \"ratclick 3\"))

This defines a binwarp-mode with additional bindings, and binds it to Super-m.

\(binwarp:define-binwarp-mode binwarp-mode
  \"M\" (:map *root-map*
      :redefine-bindings t)
  ((kbd \"a\") \"binwarp left\")
  ((kbd \"w\") \"binwarp up\")
  ((kbd \"s\") \"binwarp down\")
  ((kbd \"d\") \"binwarp left\"))

And this overrides the bindings to use WASD keys for binwarping
and start it with C-t M (in case your prefix key is C-t.)"
  `(progn
     (define-interactive-keymap ,name
         (:on-enter ,on-enter
          :on-exit ,on-exit
          :exit-on ,exit-keys)
       ,@(unless redefine-bindings *default-binwarp-keymap*)
       ,@bindings)
     (define-key ,map (kbd ,key) ,(cond ((listp name) (symbol-name (second name)))
                                        ((symbolp name) (symbol-name name))
                                        (t name)))))
