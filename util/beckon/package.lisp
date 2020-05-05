(defpackage #:beckon
  (:use #:cl)
  (:import-from #:stumpwm #:defcommand #:window-frame #:ratwarp #:current-window #:frame-x #:frame-y #:frame-height #:frame-width)
  (:export #:beckon #:*window-height-fraction* #:*window-width-fractionn*))
