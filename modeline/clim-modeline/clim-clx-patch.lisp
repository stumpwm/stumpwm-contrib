
(in-package :clim-clx)

;;;; This redefinition adds support for a dock window type, and should be pushed to mcclim propper.
;;;; Until that happens, we will include this patch here. 

;;;; We need to patch clim-clx in order to properly set the :_NET_WM_WINDOW_TYPE
;;;; to :_NET_WM_WINDOW_TYPE_DOCK.

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-xmirror top-level-sheet)))
      (case (clim-extensions:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect mirror) :on))
        (:dialog (xlib:change-property mirror
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom (xlib:window-display mirror) :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32))
	(:dock (xlib:change-property mirror
				     :_NET_WM_WINDOW_TYPE
				     (list (xlib:intern-atom (xlib:window-display mirror)
							     :_NET_WM_WINDOW_TYPE_DOCK))
				     :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x mirror) x
                (xlib:drawable-y mirror) y))
        (tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask mirror)
            (logior (xlib:window-event-mask mirror)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-xmirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for mirror)
                calling-mirror)))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window mirror)))))
