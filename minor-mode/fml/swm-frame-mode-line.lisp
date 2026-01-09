;;;; swm-frame-mode-line.lisp

(in-package #:swm-frame-mode-line)

;; We need a new struct because we have to track the mode lines parent frame
(defstruct (fml (:include stumpwm::mode-line)
                (:constructor %make-fml))
  frame)

;; And this will be a mixin for tracking the cc, and all cc objects.
(defclass frame-mode-line ()
  ((fml-ml :initform nil :accessor frame-mode-line-fml)
   (fml-cc :initform nil :accessor frame-mode-line-cc)
   (all-cc :initform nil :accessor frame-mode-line-all-cc :allocation :class)))

(defparameter *frame-mode-line-format* '("%f")
  "The frame mode line format string/list")

(stumpwm::add-screen-mode-line-formatter #\f 'format-frame-window-list)
(defun format-frame-window-list (mode-line)
  "Format the windowlist of a frame. This should only be called for fml objects"
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (stumpwm:format-with-on-click-id 
                     (let ((str (stumpwm:format-expand stumpwm:*window-formatters*
                                                       stumpwm:*window-format*
                                                       w)))
                       (if (eq w (stumpwm::frame-window (fml-frame mode-line)))
                           (stumpwm::fmt-highlight str)
                           str))
                     :ml-on-click-focus-window
                     (stumpwm::window-id w)))
                  (stumpwm::sort1
                   (stumpwm::frame-windows (stumpwm::mode-line-current-group
                                            mode-line)
                                           (if (typep mode-line 'fml)
                                               (fml-frame mode-line)
                                               (stumpwm::tile-group-current-frame
                                                (stumpwm::mode-line-current-group
                                                 mode-line))))
                   #'< :key #'stumpwm::window-number))))

(defun rendered-size-y-only (strings cc)
  "Return the y height of the rendered strings"
  (nth-value 1 (stumpwm::rendered-size strings cc)))

(defun make-fml (screen frame)
  "Create an FML mode line"
  (let* ((window (stumpwm::make-mode-line-window screen))
         (gc (stumpwm::make-mode-line-gc window screen))
         (cc (stumpwm::make-mode-line-cc window screen gc))
         (ml (%make-fml :window window
                        :screen screen
                        :head (stumpwm::frame-head
                               (stumpwm::current-group)
                               frame)
                        :format ""
                        :cc cc
                        :frame frame)))
    ml))

(defun setup-fml-window (frame cc text-height)
  "Set up the window to have the correct height"
  (let ((win (stumpwm::ccontext-win cc))
        (yoffset 0)
        (xoffset 0))
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) text-height 
            (xlib:drawable-width win) (- (stumpwm::frame-width frame)
                                          (* 2 xoffset)
                                          (* 2 stumpwm::*mode-line-border-width*))
            (xlib:window-priority win) :above
            (xlib:drawable-y win)
            (+ (- (stumpwm::frame-display-y (stumpwm:current-group)
                                            frame)
                   (rendered-size-y-only
                    (generate-frame-windows-string
                     (stumpwm:current-group)
                     frame)
                    (frame-mode-line-cc frame)))
                yoffset)
            (xlib:drawable-x win) (+ (stumpwm::frame-x frame) xoffset)))
    (xlib:map-window win)
    (xlib:display-finish-output stumpwm::*display*)))

(defun display-frame-mode-line (frame &optional force)
  "actually do the displaying of the mode line."
  (multiple-value-bind (width height)
      (stumpwm::rendered-size (stumpwm::mode-line-format
                               (frame-mode-line-fml frame))
                              (frame-mode-line-cc frame))
    (declare (ignore width))
    (setup-fml-window frame (frame-mode-line-cc frame) height))
  (let* ((stumpwm::*current-mode-line-formatters*
           stumpwm::*screen-mode-line-formatters*)
         (stumpwm::*current-mode-line-formatter-args*
           (list (frame-mode-line-fml frame)))
         (str
           (handler-case (stumpwm::mode-line-format-string
                          (frame-mode-line-fml frame))
             (error (c)
               (format nil "Unable to expand mode line format string: ~S" c)))))
    (flet ((resize-and-render (string)
             (setf (stumpwm::mode-line-contents (frame-mode-line-fml frame))
                   string)
             (stumpwm::render-strings (stumpwm::mode-line-cc
                                       (frame-mode-line-fml frame))
                                      stumpwm::*mode-line-pad-x*
                                      stumpwm::*mode-line-pad-y*
                                      (stumpwm::split-string string
                                                             (string #\Newline))
                                      ()
                                      :ml (frame-mode-line-fml frame))
             (when (stumpwm::mode-line-new-bounds (frame-mode-line-fml frame))
               (setf (stumpwm::mode-line-on-click-bounds
                      (frame-mode-line-fml frame))
                     (reverse (stumpwm::mode-line-new-bounds
                               (frame-mode-line-fml frame)))))))
      (handler-case
          (when (or force (not (string= (stumpwm::mode-line-contents
                                         (frame-mode-line-fml frame))
                                        str)))
            (resize-and-render str))
        (error (c)
          (resize-and-render
           (format nil "Unable to render mode line: ~S" c)))))))

(defun hide-frame-mode-line (frame)
  (xlib:unmap-window (stumpwm::ccontext-win (frame-mode-line-cc frame))))

(defmethod stumpwm::sync-frame-windows :after (group (frame frame-mode-line))
  (display-frame-mode-line frame))

(defmethod (setf stumpwm::frame-window) :after (new (frame frame-mode-line))
  (display-frame-mode-line frame))

(defmethod (setf stumpwm::window-frame) :around (frame (win stumpwm::tile-window))
  (let ((f (ignore-errors (stumpwm::window-frame win))))
    (call-next-method)
    (when (typep f 'frame-mode-line)
      (display-frame-mode-line f))))

(defmethod stumpwm::frame-display-height :around (group (frame frame-mode-line))
  (let ((h (call-next-method)))
    (- h (rendered-size-y-only (generate-frame-windows-string
                                (stumpwm:current-group)
                                frame)
                               (frame-mode-line-cc frame)))))

(defmethod stumpwm::frame-display-y :around (group (frame frame-mode-line))
  (let ((y (or (ignore-errors (call-next-method))
               (stumpwm::frame-y frame))))
    (+ y (rendered-size-y-only (generate-frame-windows-string
                                (stumpwm:current-group)
                                frame)
                               (frame-mode-line-cc frame)))))

(defun adjust-windows-for-frame (frame)
  (when (stumpwm::frame-window frame)
    (let* ((group (stumpwm::window-group (stumpwm::frame-window frame)))
           (windows (stumpwm::frame-windows group frame)))
      (mapc #'stumpwm::maximize-window windows))))

(defun fml-killwins (l frame)
  (declare (ignore l))
  (when (typep frame 'frame-mode-line)
    (setf (frame-mode-line-all-cc frame) (remove (frame-mode-line-cc frame)
                                                 (frame-mode-line-all-cc frame)))
    (xlib:unmap-window (stumpwm::ccontext-win (frame-mode-line-cc frame)))
    (xlib:destroy-window (stumpwm::ccontext-win (frame-mode-line-cc frame)))))

(stumpwm::add-hook stumpwm::*remove-split-hook* 'fml-killwins)

(defun fml-focus-group (new old)
  (declare (ignore old))
  (when (and (stumpwm:minor-mode-enabled-p 'frame-mode-line-bar)
             (typep new 'stumpwm::tile-group))
    (mapc (lambda (f)
            (display-frame-mode-line f))
          (alexandria:flatten (stumpwm::tile-group-frame-tree new)))))

(stumpwm:define-minor-mode frame-mode-line-bar
    (frame-mode-line stumpwm:minor-mode)
  ()
  (:global t)
  (:scope :frame-excluding-head)
  (:lighter "FML")
  (:interactive frame-mode-line-mode))

(define-frame-mode-line-bar-command update-frame-mode-line (&rest rest) ()
  "explicitly update the frame mode line"
  (declare (ignore rest))
  (let ((obj (stumpwm::tile-group-current-frame (stumpwm:current-group))))
    (display-frame-mode-line obj)))

(defmethod update-instance-for-different-class :after
    (prev (obj frame-mode-line-bar) &rest rest)
  (declare (ignore prev rest))
  (let ((fml-ml (make-fml (stumpwm::current-screen) obj)))
    (setf (frame-mode-line-cc obj) (stumpwm::mode-line-cc fml-ml)
          (frame-mode-line-fml obj) fml-ml
          (stumpwm::mode-line-format fml-ml) (list *frame-mode-line-format*)))
  (push (frame-mode-line-cc obj) (frame-mode-line-all-cc obj))
  (adjust-windows-for-frame obj)
  (when (member obj (stumpwm::flatten
                     (stumpwm::tile-group-frame-tree (stumpwm:current-group))))
    (display-frame-mode-line obj)))

(defmethod stumpwm:autodisable-minor-mode :after ((mode (eql 'frame-mode-line-bar))
                                                  obj)
  (adjust-windows-for-frame obj))

(defmethod stumpwm:autodisable-minor-mode :before ((mode (eql 'frame-mode-line-bar))
                                           (obj frame-mode-line-bar))
  (setf (frame-mode-line-all-cc obj) (remove (frame-mode-line-cc obj)
                                             (frame-mode-line-all-cc obj)))
  (xlib:unmap-window (stumpwm::ccontext-win (frame-mode-line-cc obj)))
  (xlib:destroy-window (stumpwm::ccontext-win (frame-mode-line-cc obj))))

(defun fml-change-group (new old)
  (when (typep old 'stumpwm::tile-group)
    (map nil #'hide-frame-mode-line
         (alexandria:flatten (stumpwm::tile-group-frame-tree old))))
  (when (typep new 'stumpwm::tile-group)
    (map nil #'display-frame-mode-line
         (alexandria:flatten (stumpwm::tile-group-frame-tree new)))))

(defun fml-setup-hook-fn (mode obj)
  (declare (ignore mode obj))
  (stumpwm:add-hook stumpwm:*focus-group-hook* 'fml-change-group))

(stumpwm:add-hook *frame-mode-line-bar-hook* 'fml-setup-hook-fn)

(defun fml-teardown-hook-fn (mode obj)
  (declare (ignore mode obj))
  (stumpwm:remove-hook stumpwm:*focus-group-hook* 'fml-change-group))

(stumpwm:add-hook *frame-mode-line-bar-destroy-hook* 'fml-teardown-hook-fn)

(defun autoadjust-windows-on-fml-disable (minor-mode object)
  (when (eql minor-mode 'frame-mode-line-bar)
    (adjust-windows-for-frame object)))

(stumpwm:add-hook stumpwm:*minor-mode-disable-hook*
                  'autoadjust-windows-on-fml-disable)

(defun kill-all-fml-wins (mode obj)
  (declare (ignore mode))
  (loop for cc in (frame-mode-line-all-cc obj)
        do (xlib:unmap-window (stumpwm::ccontext-win cc))
           (xlib:destroy-window (stumpwm::ccontext-win cc)))
  (setf (frame-mode-line-all-cc obj) nil))

(stumpwm:add-hook *frame-mode-line-bar-destroy-hook* 'kill-all-fml-wins)
