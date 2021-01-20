(defpackage #:stump-backlight
  (:use #:cl)
  (:export #:*current-percent*
           #:*scale*
           #:backlight-increase
           #:backlight-decrease
           #:update))
