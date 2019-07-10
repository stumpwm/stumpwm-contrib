(defpackage #:gnu-pw-mgr
  (:use #:cl #:stumpwm)
  (:export #:password-to-selection
           #:*password-id-remember-timeout*
           #:*clipboard-clear-timeout*))
