;;;; package.lisp

(defpackage #:screenshot
  (:use #:cl :stumpwm :zpng))

(in-package #:screenshot)

(import
 '(
   zpng::finish-png
   zpng::pixel-streamed-png
   zpng::start-png
   zpng::write-pixel
   ))
