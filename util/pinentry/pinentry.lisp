(defpackage #:pinentry
  (:use #:cl))

(in-package #:pinentry)

(defun main (stream)
  (let ((description (percent:decode (read-line stream)))
        (prompt (read-line stream)))
    (format stream (or (stumpwm:read-one-line (stumpwm:current-screen)
                                              (format nil "~a~%~a " description prompt)
                                              :password t)
                       ""))))

(usocket:socket-server "127.0.0.1" 22222 #'main nil
                       :in-new-thread t
                       :multi-threading t)
