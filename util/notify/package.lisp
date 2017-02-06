(defpackage #:notify
  (:use #:cl
	#:stumpwm
	#:dbus
	#:bordeaux-threads)
  (:export #:*notification-received-hook*
	   #:notify-server-toggle))
