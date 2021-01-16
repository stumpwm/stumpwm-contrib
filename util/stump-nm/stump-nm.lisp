(in-package #:stump-nm)

(stumpwm:defcommand nm-list-wireless-networks () ()
  (with-open-bus (bus (system-server-addresses))
    (let* ((root (make-root bus))
           (primary-connection (primary-connection root))
           (is-connected (= (getprop root "State") 70))
           (is-wireless (search "wireless" (getprop primary-connection "Type")))
           (device (first (connection-devices primary-connection))))
      (when (and is-connected (not is-wireless))
        (error "Wired connection is established"))
      (let ((selected
              (stumpwm:select-from-menu
               (stumpwm:current-screen)
               (mapcar (lambda (ap) (list (render-ap ap primary-connection) ap))
                       (device-access-points device)))))
        (when selected
          (let ((ap (second selected)))
            (dbus-call root "DeactivateConnection" (path primary-connection))
            (unless (active ap)
              (dbus-call root "ActivateConnection" "/" (path device) (path ap))))))))
  nil)

(stumpwm:defcommand nm-list-vpn-connections () ()
  (with-open-bus (bus (system-server-addresses))
    (let* ((root (make-root bus))
           (settings (get-settings root))
           (all-connections (get-connection-settings settings))
           (vpn-connections (remove-if-not #'is-vpn-connection all-connections))
           (active-connections (get-active-connections root))
           (active-connections-uuids (mapcar #'uuid active-connections))
           (selected (stumpwm:select-from-menu
                      (stumpwm:current-screen)
                      (mapcar (lambda (connection)
                                (let ((is-active
                                        (member (uuid connection)
                                                active-connections-uuids
                                                :test #'string=)))
                                  (list
                                   (render-vpn-connection
                                    connection
                                    (when is-active
                                      (find-active-connection-by-uuid
                                       active-connections
                                       (uuid connection))))
                                   connection
                                   is-active)))
                              vpn-connections))))
      (when selected
        (let ((connection (second selected))
              (was-active (third selected)))
          (if was-active
              (let ((active-connection (find-active-connection-by-uuid
                                        active-connections
                                        (uuid connection))))
                (dbus-call root "DeactivateConnection" (path active-connection)))
              (dbus-call root "ActivateConnection"
                         (path (connection connection)) "/" "/"))))))
  nil)

(defun find-active-connection-by-uuid (active-connections uuid)
  (find-if (lambda (active-connection)
             (string= (uuid active-connection) uuid))
           active-connections))

(defun is-vpn-connection (c)
  (string= (cs-type c) "vpn"))

(defun render-vpn-connection (connection active-connection)
  (format nil "~A ~A"
          (if active-connection
              (active-connection-state-indicator (state active-connection))
              " ")
          (id connection)))

(defun render-ap (ap connection)
  (format nil "~A ~A (~AMHz) ~A%"
          (if (active ap)
              (active-connection-state-indicator (state connection))
              " ")
          (ssid ap) (frequency ap) (strength ap)))

(defun active-connection-state-indicator (state)
  (ecase state
    (0 "?")
    (1 "?")
    (2 "*")
    (3 " ")
    (4 " ")))
