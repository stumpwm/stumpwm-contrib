;;;; swm-gaps.lisp

(in-package #:swm-gaps)

(export '(*inner-gaps-size* *outer-gaps-size* *head-gaps-size* *gaps-on* toggle-gaps))

(defvar *inner-gaps-size* 5)
(defvar *outer-gaps-size* 10)
(defvar *head-gaps-size* 0)
(defvar *gaps-on* nil)

(defun apply-gaps-p (win)
  "Tell if gaps should be applied to this window"
  (and *gaps-on* (not (stumpwm::window-transient-p win))))

(defun window-edging-p (win direction)
  "Tell if the window is touching the head in the given direction."
  (let* ((frame (stumpwm::window-frame win))
         (head (stumpwm::frame-head (stumpwm:window-group win) frame))
         (offset (nth-value 2 (stumpwm::get-edge frame direction))))
    (ecase direction
      (:top
       (= offset (stumpwm::head-y head)))
      (:bottom
       (= offset (+ (stumpwm::head-y head) (stumpwm::head-height head))))
      (:left
       (= offset (stumpwm::head-x head)))
      (:right
       (= offset (+ (stumpwm::head-x head) (stumpwm::head-width head)))))))

(defun gaps-offsets (win)
  "Return gap offset values for the window. X and Y values are added. WIDTH and
HEIGHT are subtracted."
  (let ((x *inner-gaps-size*)
        (y *inner-gaps-size*)
        (width (* 2 *inner-gaps-size*))
        (height (* 2 *inner-gaps-size*)))
    (if (window-edging-p win :top)
        (setf y (+ y *outer-gaps-size*)
              height (+ height *outer-gaps-size*)))
    (if (window-edging-p win :bottom)
        (setf height (+ height *outer-gaps-size*)))
    (if (window-edging-p win :left)
        (setf x (+ x *outer-gaps-size*)
              width (+ width *outer-gaps-size*)))
    (if (window-edging-p win :right)
        (setf width (+ width *outer-gaps-size*)))
    (values x y width height)))

(defun stumpwm::maximize-window (win)
  "Redefined gaps aware maximize function."
  (multiple-value-bind (x y wx wy width height border stick)
      (stumpwm::geometry-hints win)

    (let ((ox 0) (oy 0) (ow 0) (oh 0))
      (if (apply-gaps-p win)
          (multiple-value-setq (ox oy ow oh) (gaps-offsets win)))

      (setf width (- width ow)
            height (- height oh)
            x (+ x ox)
            y (+ y oy))

      ;; This is the only place a window's geometry should change
      (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
      (xlib:with-state ((window-parent win))
        ;; FIXME: updating the border doesn't need to be run everytime
        ;; the window is maximized, but only when the border style or
        ;; window type changes. The overhead is probably minimal,
        ;; though.
        (setf (xlib:drawable-x (window-parent win)) x
              (xlib:drawable-y (window-parent win)) y
              (xlib:drawable-border-width (window-parent win)) border)
        ;; the parent window should stick to the size of the window
        ;; unless it isn't being maximized to fill the frame.
        (if (or stick
                (find *window-border-style* '(:tight :none)))
            (setf (xlib:drawable-width (window-parent win)) (window-width win)
                  (xlib:drawable-height (window-parent win)) (window-height win))
            (let ((frame (stumpwm::window-frame win)))
              (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                                 (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                 ow)
                    (xlib:drawable-height (window-parent win)) (- (stumpwm::frame-display-height (window-group win) frame)
                                                                  (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                  oh))))
        ;; update the "extents"
        (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                              (list wx wy
                                    (- (xlib:drawable-width (window-parent win)) width wx)
                                    (- (xlib:drawable-height (window-parent win)) height wy))
                              :cardinal 32))
      (update-configuration win))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (mapcar #'stumpwm::maximize-window
          (stumpwm::only-tile-windows (stumpwm:screen-windows (current-screen)))))

;; Redefined neighbour for working with head gaps
(defun stumpwm::neighbour (direction frame frameset)
  "Returns the best neighbour of FRAME in FRAMESET on the DIRECTION edge.
   Valid directions are :UP, :DOWN, :LEFT, :RIGHT.
   eg: (NEIGHBOUR :UP F FS) finds the frame in FS that is the 'best'
   neighbour above F."
  (let ((src-edge (ecase direction
                    (:up :top)
                    (:down :bottom)
                    (:left :left)
                    (:right :right)))
        (opposite (ecase direction
                    (:up :bottom)
                    (:down :top)
                    (:left :right)
                    (:right :left)))
        (best-frame nil)
        (best-overlap 0)
        (nearest-edge-diff nil))
    (multiple-value-bind (src-s src-e src-offset)
        (stumpwm::get-edge frame src-edge)

      ;; Get the edge distance closest in the required direction
      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (stumpwm::get-edge f opposite)
          (let ((offset-diff (abs (- src-offset offset))))
            (if nearest-edge-diff
                (if (< offset-diff nearest-edge-diff)
                    (setf nearest-edge-diff offset-diff))
                (setf nearest-edge-diff offset-diff)))))

      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (stumpwm::get-edge f opposite)
          (let ((overlap (- (min src-e e)
                            (max src-s s))))
            ;; Two edges are neighbours if they have the same offset (after
            ;; accounting for gaps) and their starts and ends overlap. We want
            ;; to find the neighbour that overlaps the most.
            (when (and (= (abs (- src-offset offset)) nearest-edge-diff)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun add-head-gaps ()
  "Add extra gap to the head boundary"
  (mapcar (lambda (head)
            (let* ((height (stumpwm::head-height head))
                   (width (stumpwm::head-width head))
                   (x (stumpwm::head-x head))
                   (y (stumpwm::head-y head))
                   (gap *head-gaps-size*)
                   (new-height (- height (* 2 gap)))
                   (new-width (- width (* 2 gap))))
              (stumpwm::resize-head
               (stumpwm::head-number head)
               (+ x gap) (+ y gap)
               new-width new-height)))
          (screen-heads (current-screen))))

(defcommand toggle-gaps () ()
  "Toggle gaps"
  (setf *gaps-on* (null *gaps-on*))
  (if *gaps-on*
      (progn
        (add-head-gaps)
        (reset-all-windows))
      (stumpwm:refresh-heads)))
