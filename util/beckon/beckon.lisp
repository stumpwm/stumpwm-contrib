(in-package #:beckon)

(defvar *window-height-fraction* 0.5
  "height from the top of the frame")

(defvar *window-width-fraction* 0.5
  "width from the top of the frame")

(defcommand beckon () ()
  "Beckon the mouse to the current window"
  (with-accessors ((x frame-x)
                   (y frame-y)
                   (height frame-height)
                   (width frame-width))
      (window-frame (current-window))
    (ratwarp
     (round
      (+ x (* width *window-height-fraction*)))
     (round
      (+ y (* height *window-width-fraction*))))))
