;;;; xinput-toggle.lisp

(in-package #:xinput-toggle)

;;; Module to toggle (on/off) xinput devices (touchpad, mouse, etc.)
;;; Copyright 2025 Kayomarz Gazder.

(defvar *case-insensitive-regex* t
  "Device names are specified via a regex. This variable decides if the
regex match should be case insensitive or not.")

(defvar *exclude-keyboards* t
  "When using a regex to match devices to turn on or off, this variable
guards against accidental disabling of keyboards (xinput devices with
\"keyboard\" in their name). For instance, the regex \"\" matches all devices
including the keyboards. Disabling keyboards may render StumpWM unusable.

By default, this variable is set to T and devices containing \"keyboard\" will
be excluded from the set of matched devices.

When set to NIL, the guard is removed and device names containing \"keyboard\"
will be part of the set of devices. Hence keyboards are at the risk of being
disabled.

Disabling keyboards may render StumpWM unusable.

Use with caution.

PS: To test what device names matche a regex, use the function
XINPUT-LIST-DEVICES, passing regex arg.")

(defvar *whitespaces*
  (list #\Space #\Tab #\Newline #\Return #\Linefeed #\Page)
  "Characters considered as whitespace when parsing an xinput line.")

(defclass device ()
  ((id :reader device-id :initarg :id)
   (name :reader device-name :initarg :name)
   (info :reader device-info :initarg :info)))

(defmethod name-matches-p ((d device) regex)
  "Returns T if device name matches REGEX, else returns NIL."
  (let ((scanner (ppcre:create-scanner
                  regex :case-insensitive-mode *case-insensitive-regex*)))
    (when (cl-ppcre:scan-to-strings scanner (device-name d))
      t)))

(defmethod is-keyboard-p ((d device))
  "Returns T if device name contains string \"keyboard\", else returns NIL."
  (name-matches-p d "keyboard"))

(defun make-device-from-line (line)
  "Make a device object by parsing a text line representing a device .

An example of a line representing a device is:
\"FTCS1000:00 2808:0101 Touchpad\" \"13\" \"[floating slave]\""
  (let ((parts (multiple-value-bind (_ parts)
                   (ppcre:scan-to-strings
                    "^[^\\w]*(.*)id\\s*=\\s*(\\d*)(.*)"
                    line)
                 (declare (ignore _))
                 (map 'vector
                      (lambda (s) (string-trim *whitespaces* s))
                      parts))))
    (make-instance 'device :name (elt parts 0)
                           :id (parse-integer (elt parts 1))
                           :info (elt parts 2))))

(defmethod enabledp ((d device))
  (xinput-enabled-p (device-id d)))

(defmethod disable ((d device))
  "Enables device and returns a string indicating the action."
  (xinput-cmd "disable" (device-id d))
  (format nil "disabled ~A" d))

(defmethod enable ((d device))
  "Enables device and returns a string indicating the action."
  (xinput-cmd "enable" (device-id d))
  (format nil "enabled ~A" d))

(defmethod toggle ((d device))
  "Toggles the device and returns a string indicating if it was enabled or
disabled along with the device name and id."
  (if (enabledp d) (disable d) (enable d)))

(defmethod print-object ((d device) stream)
  (format stream "~A (device ~A)"
          (device-name d)
          (device-id d)))

(defun xinput-cmd (&rest args)
  "Run the `xinput` utility program with args. For example:
  (xinput-cmd \"enable\" 10) ; enable device id 10

The equivalent shell command would be:
  xinput enable 10"
  (run-shell-command (format nil "xinput ~{~A~^ ~}" args) t))

(defun xinput-error ()
  (error (format nil
            "xinput error.~%Make sure 'xinput' is installed.")))

(defun get-all-devices ()
  (let ((stdout (xinput-cmd "list" "--short")))
    (if (eq (length stdout) 0) ;; something went wrong if no stdout
        (xinput-error)
        (mapcar
         (lambda (line)
           (make-device-from-line line))
         (ppcre:split #\Newline stdout)))))

(defun sort-devices (device-list)
  (sort device-list
        (lambda (d1 d2)
          (< (device-id d1) (device-id d2)))))

(defun get-devices (&optional (regex nil))
  "Get a list of devices whose name match regex.  Each device is represented by
the DEVICE class."
  (sort-devices
   (let* ((all-devices (get-all-devices))
          (devices  (if (not regex)
                        all-devices
                        (remove-if-not (lambda (d) (name-matches-p d regex))
                                       all-devices))))
     (if *exclude-keyboards*
         (remove-if (lambda (d) (is-keyboard-p d))
                    devices)
         devices))))

(defun xinput-enabled-p (device-id)
  "Returns T if the device is enabled, else NIL."
  (let* ((scanner (ppcre:create-scanner "device\\s*enabled[^:]*:\\s*(\\d*)"
                                        :case-insensitive-mode t))
         (xinput-stdout (xinput-cmd "list-props" device-id))
         (groups (nth-value 1 (ppcre:scan-to-strings scanner xinput-stdout)))
         (status (elt groups 0)))
    (equal status "1")))

(defun for-devices (name-regex fn)
  "For each device matching NAME-REGEX, call fn, passing the device as arg."
  (let ((devices (get-devices name-regex)))
    (if (null devices)
        (message "xinput-toggle: No devices match regex \"~A\"." name-regex)
        (message (format nil "~{~A~^~%~}"
                         (mapcar (lambda (d) (funcall fn d)) devices))))))

(defcommand xinput-list-devices (name-regex) ((:string))
  "List xinput devices whose names match NAME-REGEX.

If NAME-REGEX is empty, all devices are listed. By default, devices containing
'keyboard' in their name are excluded unless `*exclude-keyboards*` is set to
NIL."
  (message (format nil "xinput: Devices matching regex \"~A\":~% ~%~{~A~^~%~}"
                   name-regex
                   (mapcar #'princ-to-string (get-devices name-regex)))))

(defcommand xinput-enable-devices (name-regex) ((:string))
  "Enable xinput devices whose names match NAME-REGEX.

NAME-REGEX is a regular expression used to match device names. By default,
devices containing 'keyboard' (case-insensitive) in their name are excluded to
avoid accidental disabling of keyboards. Set `*exclude-keyboards*` to NIL to
include keyboards.

To view xinput device names on your system, use the shell command:
  xinput --list --name-only

For example to toggle devices whoose name contains \"touchpad\":

  (enable-devices \"touchpad\")"
  (for-devices name-regex (lambda (d) (enable d))))


(defcommand xinput-disable-devices (name-regex) ((:string))
  "Disable xinput devices whose names match NAME-REGEX.

Also see documentation of `enable-devices`"
  (for-devices name-regex (lambda (d) (disable d))))

(defcommand xinput-toggle-devices (name-regex) ((:string))
  "Toggle the state of xinput devices whose names match NAME-REGEX.

Also see documentation of `enable-devices`"
  (for-devices name-regex (lambda (d) (toggle d))))
