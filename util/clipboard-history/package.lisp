(defpackage #:clipboard-history
  (:use #:cl)
  (:export #:start-clipboard-manager
           #:stop-clipboard-manager
           #:show-clipboard-history
           #:*clipboard-history-max-length*))
