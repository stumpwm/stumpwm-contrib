(asdf:defsystem "notify"
    :serial t
    :description "DBus-based notification server part"
    :author "Slava Barinov <rayslava@gmail.com>"
    :license "GPLv3"
    :depends-on ("stumpwm"
                 "xml-emitter"
                 "dbus"
                 "bordeaux-threads")
    :components ((:file "package")
                 (:file "notify")))
