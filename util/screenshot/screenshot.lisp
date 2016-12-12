(in-package #:screenshot)

(export '(screenshot screenshot-window))

(defun %screenshot-window (drawable file &key (height (xlib:drawable-height drawable))
                                           (width (xlib:drawable-width drawable)))
  (let* ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (multiple-value-bind (pixarray depth visual)
        (xlib:get-raw-image drawable :x 0 :y 0 :width width :height height
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
