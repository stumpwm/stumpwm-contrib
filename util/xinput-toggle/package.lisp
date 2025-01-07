;;;; package.lisp

(defpackage #:xinput-toggle
  (:use #:cl)
  (:import-from #:stumpwm
                #:defcommand
                #:message
                #:run-shell-command)
  (:export #:*case-insensitive-regex*
           #:*exclude-keyboards*
           #:xinput-disable-devices
           #:xinput-enable-devices
           #:xinput-list-devices
           #:xinput-toggle-devices))
