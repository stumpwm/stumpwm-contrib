;;;; wacom.lisp

(in-package #:wacom)

(defparameter *portrait-rotate* "ccw"
  "If you notice the directions are the reverse of what you desirte when your
   frame is in portrait orientation, then change this parameter to 'cw'.")

(defparameter *landscape-rotate* "half"
  "If you notice directions are the reverse of what you desire when your
   frame is in landscape orientation, then change this parameter to 'none'.")

(defun current-frame ()
  (stumpwm::tile-group-current-frame (stumpwm:current-group)))

(defun tablet-id ()
  "List devices with `xsetwacom --list devices` results in something like
   the following output.

   Wacom One by Wacom S Pen stylus         id: 13  type: STYLUS
   Wacom One by Wacom S Pen eraser         id: 14  type: ERASER

   I'm not sure how type is determined. Messing around in my
   local environment I found that the type that I need to
   configure is the `STYLUS`.

   So this function returns the ID of the STYLUS device."
  (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
      (declare (ignore match))
      (aref groups 0))))

(defun reset-area! ()
  "Resets tablet area to system default."
  (run-shell-command (format nil "xsetwacom set ~A ResetArea" (tablet-id)) t))

(defun tablet-area ()
  "Returns (top-offset left-offset width height)."
  (mapcar
   #'parse-integer
   (cl-ppcre:split
    "\\s+"
    (run-shell-command (format nil "xsetwacom get ~A Area" (tablet-id)) t))))

(defun current-frame-dimensions ()
  (let* ((curframe (current-frame))
         (top (stumpwm:frame-y curframe))
         (left (stumpwm:frame-x curframe))
         (width (stumpwm:frame-width curframe))
         (height (stumpwm:frame-height curframe)))
    (list top left width height)))

(defun orientation ()
  "Returns either :portrait or :landscape. based on dimensions of current frame."
  (destructuring-bind (top left width height)
      (current-frame-dimensions)
    (declare (ignore top left)) ;; Only height used to get sync ratios
    (if (> height width)
        :portrait
        :landscape)))

(defun reset-rotate! ()
  (rotate! "none"))

(defun rotate! (orientation)
  (run-shell-command (format nil "xsetwacom set ~A rotate ~A" (tablet-id) orientation) t))

(defun -wacom-cur-frame ()
  (destructuring-bind (top left width height)
      (current-frame-dimensions)
    (let ((ratio (/ width height)))
      (reset-area!)
      (destructuring-bind
          (tablet-top tablet-left tablet-width tablet-height)
          (tablet-area)
        (declare (ignore tablet-left tablet-top)) ;; Only height used to get sync ratios
        (let* ((id (tablet-id))
               (ideal-width (* tablet-height ratio))
               (discount-ratio (min 1 (/ tablet-width ideal-width)))
               (target-width (min tablet-width ideal-width))
               (target-height (* discount-ratio tablet-height))
               (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                    id (round target-width) target-height))
               ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
               (map-to-output (format nil "xsetwacom set ~A MapToOutput ~Ax~A+~A+~A" id width height left top)))
          (run-shell-command stylus-area t)
          (run-shell-command map-to-output t)
          (rotate!
           (if (eq (orientation) :portrait)
               *portrait-rotate*
               *landscape-rotate*)))))))

(defcommand map-wacom-to-current-frame () ()
  (-wacom-cur-frame))
