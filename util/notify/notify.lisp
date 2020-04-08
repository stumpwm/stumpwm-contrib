(in-package #:notify)

(export
 '(*notify-server-max-title-lines*
   *notify-server-max-body-lines*
   *notify-server-max-line-length*
   *notify-server-title-color*
   *notify-server-body-color*))

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

(defparameter *notify-server-max-title-lines* 2)
(defparameter *notify-server-max-body-lines* 20)
(defparameter *notify-server-max-line-length* 100)

(defparameter *notify-server-title-color* "^4")
(defparameter *notify-server-body-color* "^0")

(defun flatten-once (lstst)
  (mapcan (lambda (x) x) lstst))

(defun rewrap-line (line)
  (if (> (length line) *notify-server-max-line-length*)
      (cons (subseq line 0 *notify-server-max-line-length*)
            (rewrap-line (subseq line *notify-server-max-line-length*)))
      (list line)))

(defun rewrap-body (body &key (max-lines 20) (show-ellipsis nil))
  (let ((lines (split-sequence:split-sequence #\newline body)))
    (format nil "~{~A~^~%~}"
            (multiple-value-bind (fst rst)
                (stumpwm::take max-lines
                               (flatten-once
                                (mapcar #'rewrap-line lines)))
              (if (and rst show-ellipsis)
                  (append fst (list "..."))
                  fst)))))

(defun show-notification (app icon summary body)
  "Show the notification using standard STUMPWM::MESSAGE function"
  (declare (ignore app icon))
  (stumpwm:message "~A~A^0~% ~%~A~A^0"
                   *notify-server-title-color*
                   (rewrap-body
                    summary
                    :max-lines *notify-server-max-title-lines*
                    :show-ellipsis t)
                   *notify-server-body-color*
                   (rewrap-body
                    body
                    :max-lines *notify-server-max-body-lines*
                    :show-ellipsis t)))

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

(define-dbus-method (notify-dbus-service get-capabilities) ()
    ((:array :string))
  (:interface "org.freedesktop.Notifications")
  (:name "GetCapabilities")
  (values #("actions" "action-icons" "body" "body-markup")))

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
