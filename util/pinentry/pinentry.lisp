(defpackage #:pinentry
  (:use #:cl))

(in-package #:pinentry)

(defun main (stream)
  (format stream (or (stumpwm:read-one-line (stumpwm:current-screen)
                                            "Passphrase: "
                                            :password t)
                     "")))

(usocket:socket-server "127.0.0.1" 22222 #'main nil
                       :in-new-thread t
                       :multi-threading t)
