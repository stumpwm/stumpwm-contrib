(in-package #:stump-nm)

(defmacro prog1-let (declaration &body body)
  `(let (,declaration)
     (prog1 ,(first declaration)
       ,@body)))

(defvar *service* "org.freedesktop.NetworkManager")

(defun getprop (object prop)
  "Thin wrapper over DBUS:GET-PROPERTY that works with our DBUS-OBJECT instances."
  (get-property (bus object) *service* (path object) (interface object) prop))

(defun dbus-call (object method &rest args)
  "Thin wrapper over DBUS:OBJECT-INVOKE that works with our DBUS-OBJECT instances."
  (let ((dbus-object (make-object-from-introspection
                      (bus-connection (bus object))
                      (path object)
                      *service*)))
    (apply #'object-invoke dbus-object (interface object) method args)))

(defclass dbus-object ()
  ((path :initarg :path :reader path)
   (bus :initarg :bus :reader bus)
   (interface :reader interface)))

(defclass root-object (dbus-object)
  ((path :initform "/org/freedesktop/NetworkManager")
   (interface :initform "org.freedesktop.NetworkManager")))

(defun make-root (bus)
  (make-instance 'root-object :bus bus))

(defclass settings (dbus-object)
  ((interface :initform "org.freedesktop.NetworkManager.Settings")))

(defclass setting-connection (dbus-object)
  ((interface :initform "org.freedesktop.NetworkManager.Settings.Connection")))

(defun get-settings (root)
  (make-instance 'settings
                 :path "/org/freedesktop/NetworkManager/Settings"
                 :bus (bus root)))

(defclass connection-settings ()
  ((id :initarg :id :reader id)
   (uuid :initarg :uuid :reader uuid)
   (connection :initarg :connection :reader connection)
   (type :initarg :type :reader cs-type)))

(defun get-connection-settings (settings)
  (mapcar (lambda (connection-path)
            (block lambda
              (let* ((connection (make-instance 'setting-connection
                                                :path connection-path
                                                :bus (bus settings)))
                     (connection-settings (dbus-call connection "GetSettings")))
                (dolist (setting connection-settings)
                  (when (string= (first setting) "connection")
                    (return-from lambda
                      (let ((settings (second setting)))
                        (make-instance
                         'connection-settings
                         :connection connection
                         :id (first (assoc-value settings "id" :test #'string=))
                         :uuid (first (assoc-value settings "uuid" :test #'string=))
                         :type (first (assoc-value settings "type" :test #'string=))))))))))
          (getprop settings "Connections")))

(defclass wireless-device (dbus-object)
  ((interface :initform "org.freedesktop.NetworkManager.Device.Wireless")))

(defclass access-point (dbus-object)
  ((interface :initform "org.freedesktop.NetworkManager.AccessPoint")
   (active :initarg :active :reader active)
   (ssid :accessor ssid)
   (frequency :accessor frequency)
   (strength :accessor strength)))

(defun device-access-points (device)
  (with-introspected-object (object (bus device) (path device) *service*)
    (let ((active-access-point (getprop device "ActiveAccessPoint")))
      (mapcar (lambda (access-point-path)
                (prog1-let (ap (make-instance 'access-point
                                              :path access-point-path
                                              :bus (bus device)
                                              :active (string= access-point-path
                                                               active-access-point)))
                  (setf (ssid ap) (access-point-ssid ap)
                        (frequency ap) (getprop ap "Frequency")
                        (strength ap) (getprop ap "Strength"))))
              (object (interface device) "GetAllAccessPoints")))))

(defun access-point-ssid (ap)
  (babel:octets-to-string
   (let ((ssid (getprop ap "Ssid")))
     (make-array (length ssid) :element-type '(unsigned-byte 8) :initial-contents ssid))))

(defclass active-connection (dbus-object)
  ((interface :initform "org.freedesktop.NetworkManager.Connection.Active")
   (uuid :accessor uuid)
   (state :accessor state)))

(defun get-active-connections (root)
  (mapcar (lambda (connection-path)
            (make-active-connection root connection-path))
          (getprop root "ActiveConnections")))

(defun make-active-connection (root connection-path)
  (prog1-let (active-connection (make-instance
                                 'active-connection
                                 :path connection-path
                                 :bus (bus root)))
    (setf (uuid active-connection) (getprop active-connection "Uuid")
          (state active-connection) (getprop active-connection "State"))))

(defun primary-connection (root)
  (make-active-connection root (getprop root "PrimaryConnection")))

(defun connection-devices (connection)
  (mapcar
   (lambda (device-path)
     (make-instance 'wireless-device
                    :bus (bus connection)
                    :path device-path))
   (getprop connection "Devices")))
