;;;; swm-gaps.lisp

(in-package #:swm-gaps)

(export '(*inner-gaps-size* *outer-gaps-size* *gaps-on* toggle-gaps))

(defvar *inner-gaps-size* 5)
(defvar *outer-gaps-size* 10)
(defvar *gaps-on* nil)

;; Redefined - with `if`s for *inner-gaps-on*
(defun stumpwm::maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y wx wy width height border stick)
      (stumpwm::geometry-hints win)

    (if (and *gaps-on* (not (stumpwm::window-transient-p win)))
        (setf width (- width (* 2 *inner-gaps-size*))
              height (- height (* 2 *inner-gaps-size*))
              x (+ x *inner-gaps-size*)
              y (+ y *inner-gaps-size*)))

    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
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
                                                               (if (and *gaps-on* (not (stumpwm::window-transient-p win))) (* 2 *inner-gaps-size*) 0))
                  (xlib:drawable-height (window-parent win)) (- (stumpwm::frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                (if (and *gaps-on* (not (stumpwm::window-transient-p win))) (* 2 *inner-gaps-size*) 0)))))
      ;; update the "extents"
      (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                            (list wx wy
                                  (- (xlib:drawable-width (window-parent win)) width wx)
                                  (- (xlib:drawable-height (window-parent win)) height wy))
                            :cardinal 32))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (let ((windows (mapcan (lambda (g)
                           (mapcar (lambda (w) w) (stumpwm::sort-windows g)))
                         (stumpwm::sort-groups (current-screen)))))
    (mapcar (lambda (w)
              (if (string= (class-name (class-of w)) "TILE-WINDOW")
                  (stumpwm::maximize-window w))) windows)))

;; Redefined neighbour for working with outer gaps
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
            ;; Two edges are neighbours if they have the same offset and their starts and ends
            ;; overlap.  We want to find the neighbour that overlaps the most.
            (when (and (= (abs (- src-offset offset)) nearest-edge-diff)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun add-outer-gaps ()
  "Add extra gap to the outermost borders"
  (mapcar (lambda (head)
            (let* ((height (stumpwm::head-height head))
                   (width (stumpwm::head-width head))
                   (x (stumpwm::head-x head))
                   (y (stumpwm::head-y head))
                   (gap *outer-gaps-size*)
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
        (add-outer-gaps)
        (reset-all-windows))
      (stumpwm::refresh-heads)))
