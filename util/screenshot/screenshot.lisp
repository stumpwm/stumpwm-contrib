(in-package #:screenshot)

(export '(screenshot screenshot-window screenshot-area))

(defun colorname-to-color (colorname)
  (let* ((screen (stumpwm:current-screen))
         (colormap (xlib:screen-default-colormap (stumpwm:screen-number screen)))
         (color (xlib:lookup-color colormap colorname)))
    (xlib:alloc-color colormap color)))

(defun %screenshot-window (drawable file
                           &key
                             (x 0)
                             (y 0)
                             (height (xlib:drawable-height drawable))
                             (width (xlib:drawable-width drawable)))
  (let* ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (multiple-value-bind (pixarray depth visual)
        (xlib:get-raw-image drawable :x x :y y :width width :height height
                                     :format :Z-PIXMAP)
      (declare (ignore depth visual))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (start-png png stream)
        (case (xlib:display-byte-order (xlib:drawable-display drawable))
          (:lsbfirst
           (do ((i 0 (+ 4 i)))
               ((>= i (length pixarray)))
             (write-pixel (list (aref pixarray (+ 2 i))
                                (aref pixarray (+ 1 i))
                                (aref pixarray i)
                                #xFF)
                          png)))
          (:msbfirst
           (do ((i 0 (+ 4 i)))
               ((>= i (* height width 4)))
             (write-pixel (list (aref pixarray (1+ i))
                                (aref pixarray (+ 2 i))
                                (aref pixarray (+ 3 i))
                                #xFF)
                          png))))
        (finish-png png)))))

(defun clamp-xy (x1 y1 x2 y2)
  (values (max x1 0)
          (max y1 0)
          (max (+ 2 x1) x2)
          (max (+ 2 y1) y2)))

(stumpwm:defcommand screenshot-area
    (filename)
    ((:rest "Filename: "))
  "Make screenshot of selected area of display."
  (let ((display stumpwm:*display*)
        (x1 0)
        (y1 0)
        (x2 0)
        (y2 0))
    ;; The secret to drawing the selection rectangle comes from the xor function in the graphics context
    ;; and the subwindow-mode :include-inferiors.
    ;; https://askubuntu.com/questions/487725/how-can-i-draw-selection-rectangle-on-screen-for-my-script
    (let* ((window (xlib:create-window
                    :parent (xlib:screen-root (xlib:display-default-screen display))
                    :x 0
                    :y 0
                    :width (stumpwm:screen-width (stumpwm:current-screen))
                    :height (stumpwm:screen-height (stumpwm:current-screen))
                    :background :none
                    :event-mask '(:exposure :button-press :button-release)))
           (gc (xlib:create-gcontext
                :drawable window
                :line-width 1
                :foreground (colorname-to-color "green")
                :function boole-xor
                :subwindow-mode :include-inferiors)))
      (unwind-protect
           (progn
             (xlib:map-window window)
             (xlib:grab-pointer window '(:button-press :button-release :button-motion) :owner-p t)
             (stumpwm:echo "Click and drag the area to screenshot.")
             (xlib:event-case (display :discard-p t)
               (exposure
                ()
                nil #| continue receiving events |#)
               (motion-notify
                (root-x root-y)
                (multiple-value-bind (x1 y1 root-x root-y) (clamp-xy x1 y1 root-x root-y)
                  ;; Since we're using that boole-xor function in the graphics context,
                  ;; drawing over the old rectangle reverts the pixels back to their original values.
                  (when x2
                    (xlib:draw-rectangle window gc x1 y1 (- x2 x1) (- y2 y1)))
                  (setf x2 root-x)
                  (setf y2 root-y)
                  ;; Now draw the new rectangle.
                  (xlib:draw-rectangle window gc x1 y1 (- root-x x1) (- root-y y1)))
                nil)
               (button-press
                ()
                (multiple-value-bind (root-x root-y) (xlib:global-pointer-position display)
                  (stumpwm:echo (format nil "Screenshotting from ~A, ~A to ..." root-x root-y))
                  (setf x1 root-x)
                  (setf y1 root-y)
                  (setf x2 (+ 1 x1))
                  (setf y2 (+ 1 y1))
                  (xlib:draw-rectangle window gc x1 y1 (- x2 x1) (- y2 y1))
                  nil))
               (button-release
                ()
                (multiple-value-bind (root-x root-y) (xlib:global-pointer-position display)
                  (multiple-value-bind (x1 y1 root-x root-y) (clamp-xy x1 y1 root-x root-y)
                    ;; Since we're using that boole-xor function in the graphics context,
                    ;; drawing over the old rectangle reverts the pixels back to their original values.
                    (when x2
                      (xlib:draw-rectangle window gc x1 y1 (- x2 x1) (- y2 y1)))
                    (stumpwm:echo (format nil "Screenshotted from ~A, ~A to ~A, ~A to ~A" x1 y1 root-x root-y filename))
                    (%screenshot-window (xlib:screen-root (stumpwm:screen-number (stumpwm:current-screen))) filename
                                        :x (- x1 1)
                                        :y (- y1 1)
                                        :width (- (- root-x x1) 1)
                                        :height (- (- root-y y1) 1))
                    (xlib:ungrab-pointer display)
                    (xlib:destroy-window window))
                  t))))
        (xlib:destroy-window window)))))

(stumpwm:defcommand screenshot
    (filename)
  ((:rest "Filename: "))
  "Make screenshot of root window"
  (%screenshot-window (xlib:screen-root (stumpwm:screen-number (stumpwm:current-screen))) filename))

(stumpwm:defcommand screenshot-window
    (filename)
  ((:rest "Filename: "))
  "Make screenshot of focus window"
  (%screenshot-window (stumpwm:window-xwin (stumpwm:current-window)) filename))
