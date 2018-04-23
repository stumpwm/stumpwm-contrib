(in-package #:notify)

;;;;
;;;; Notify server to show standard notifications messages
;;;;

(defvar *notification-received-hook* '(show-notification)
  "Function to execute when notification received")

(defvar *notify-server-is-on* nil
  "Does notify-server listen to notifications?")

(defvar *notify-server-thread* nil
  "DBus listening thread")

(defparameter *notify-server-start-message*
  "Notification Server listening for notifications.")

(defparameter *notify-server-stop-message*
  "Notification Server will now stop listening for notifications.")

(defun show-notification (app icon summary body)
  "Show the notification using standard STUMPWM::MESSAGE function"
  (declare (ignore app icon))
  (stumpwm:message "~A ~A" summary body))

(define-dbus-object notify-dbus-service
    (:path "/org/freedesktop/Notifications"))

(define-dbus-method (notify-dbus-service notify)
    ((appName :string) (id :uint32) (icon :string)
     (summary :string) (body :string)
     (actions (:array :string))
     (hints (:array (:dict-entry :string :variant)))
     (timeout :int32))
  (:uint32)
  (:interface "org.freedesktop.Notifications")
  (:name "Notify")
  (declare (ignore id actions hints timeout))
  (stumpwm:run-hook-with-args *notification-received-hook* appName icon summary body)
  (values 1))

(define-dbus-method (notify-dbus-service get-server-information) ()
  (:string :string :string :string)
  (:interface "org.freedesktop.Notifications")
  (:name "GetServerInformation")
  (values "StumpWM" "StumpWM" "localhost" "0.1"))

(defun notifications-listen ()
  (handler-case
      (with-open-bus (bus (session-server-addresses))
	(with-introspected-object
	    (notifications bus "/org/freedesktop/DBus" "org.freedesktop.DBus")
	  (notifications "org.freedesktop.DBus" "RequestName"
			 "org.freedesktop.Notifications" 0))
	(publish-objects bus))
    (end-of-file ()
      :disconnected-by-bus)))

(defun notify-server-on ()
  "Turns on notify server."
  (unless *notify-server-is-on*
    (setf *notify-server-thread*
          (make-thread #'notifications-listen :name "listener"))
    (setf *notify-server-is-on* t)
    (stumpwm:message *notify-server-start-message*)))

(defun notify-server-off ()
  "Turns off notify server"
  (destroy-thread *notify-server-thread*)
  (setf *notify-server-is-on* nil)
  (stumpwm:message *notify-server-stop-message*))

(stumpwm:defcommand notify-server-toggle () ()
  "Toggles notify server."
  (if *notify-server-is-on*
      (notify-server-off)
      (notify-server-on)))
