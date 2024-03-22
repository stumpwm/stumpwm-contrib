(defpackage #:pinentry
  (:use #:cl)
  (:export #:getpin))

(in-package #:pinentry)

(defun getpin (description prompt)
  (ignore-errors
   (let ((description (percent:decode description))
         (prompt (percent:decode prompt)))
     (percent:encode
      (or (stumpwm:read-one-line (stumpwm:current-screen)
                                 (format nil "~a~%~a " description prompt)
                                 :password t)
          "")))))
