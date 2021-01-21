(defpackage #:stump-backlight
  (:use #:cl)
  (:export #:*default-percent*
           #:*scale*
           #:backlight-increase
           #:backlight-decrease
           #:current-output
           #:update))
