(in-package #:clim-clx)

;;;;;;;;;;;;;
;;; Patch ;;;
;;;;;;;;;;;;;

;; We need to patch this to support :dock frame types. 

(defmethod adopt-frame :after
    ((fm clx-frame-manager) (frame standard-application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-mirror top-level-sheet))
           (window (window mirror)))
      (case (clime:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect window) :on))
        (:dock (xlib:change-property window
				     :_NET_WM_WINDOW_TYPE
				     (list (xlib:intern-atom
                                            (xlib:window-display window)
                                            :_NET_WM_WINDOW_TYPE_DOCK))
				     :atom 32))
        (:dialog (xlib:change-property window
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom
                                              (xlib:window-display window)
                                              :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x window) x
                (xlib:drawable-y window) y))
        (tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask window)
            (logior (xlib:window-event-mask window)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-mirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for window) (window calling-mirror))))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window window)))))
