(defpackage #:notify
  (:use #:cl
	#:dbus
	#:bordeaux-threads)
  (:export #:*notification-received-hook*
	   #:notify-server-toggle))
