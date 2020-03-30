(in-package :common-lisp-user)

(defpackage #:stump-radio
  (:use #:cl #:stumpwm)
  (:export ;; commands:
           #:radio-start #:radio-stop #:radio-toggle-playback #:radio-force-restart
           #:radio-next-station #:radio-previous-station #:radio-list-stations
           ;; functions:
           #:add-station #:remove-station #:list-stations #:clear-stations))
