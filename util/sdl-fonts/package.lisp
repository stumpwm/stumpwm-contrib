;;;; package.lisp

(defpackage #:sdl-fonts
  (:use #:cl))

(in-package #:sdl-fonts)

(import '(stumpwm::font-exists-p
          stumpwm::open-font
          stumpwm::close-font
	  stumpwm::font-ascent
          stumpwm::font-descent
          stumpwm::text-line-width
	  stumpwm::draw-image-glyphs
          stumpwm::font-height
          cffi:define-foreign-library
          cffi:use-foreign-library
          cffi:defcstruct
          cffi:defcfun
          cffi:foreign-slot-value
          cffi:with-foreign-object
          cffi:foreign-type-size
          cffi:null-pointer-p
          cffi:mem-ref
          cffi:inc-pointer))
