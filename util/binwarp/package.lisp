;;;; package.lisp

(defpackage #:binwarp
  (:use #:cl :stumpwm)
  (:export #:*binwarp-mode-p*
           #:*binwarp-area*
           #:*binwarp-history*
           #:*reinitiate-ptr*
           #:*init-ptr-position*
           #:*preserve-history*
           #:*default-binwarp-keymap*
           ;; Utils
           #:with-pointer
           #:randwarp
           #:define-binwarp-mode
           ;; Binwarping commands
           #:init-binwarp
           #:exit-binwarp
           #:back-binwarp
           #:binwarp))
