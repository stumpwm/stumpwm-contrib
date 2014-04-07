;;;; package.lisp

(defpackage #:ttf-fonts
  (:use #:cl #:stumpwm))

(in-package #:ttf-fonts)

(import '(stumpwm::font-exists-p stumpwm::open-font stumpwm::close-font
	 stumpwm::font-ascent stumpwm::font-descent stumpwm::text-line-width
	 stumpwm::draw-image-glyphs stumpwm::font-height))
