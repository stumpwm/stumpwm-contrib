;;;; ttf-fonts.lisp

(in-package #:ttf-fonts)
(export '(font-exists-p 
	  open-font
	  close-font
	  font-ascent
	  font-descent
	  text-line-width
	  draw-image-glyphs))
;;; "ttf-fonts" goes here. Hacks and glory await!
;;;; TTF fonts
(defmethod font-exists-p ((font xft:font))
  ;; if we can list the font then it exists
  t)

(defmethod open-font ((display xlib:display) (font xft:font))
  font)

(defmethod close-font ((font xft:font)))

(defmethod font-ascent ((font xft:font))
  (xft:font-ascent (screen-number (current-screen)) font))

(defmethod font-descent ((font xft:font))
  (xft:font-descent (screen-number (current-screen)) font))

(defmethod font-height ((font xft:font))
  (+ (font-ascent font)
     (- (font-descent font))))

(defmethod text-line-width ((font xft:font) text &rest keys &key (start 0) end translate)
  (declare (ignorable start end translate))
  (apply 'xft:text-line-width (screen-number (current-screen)) font text
         :allow-other-keys t keys))

(defmethod draw-image-glyphs (drawable 
                              gcontext
                              (font xft:font)
                              x y
                              sequence &rest keys
			      &key (start 0) end translate width size)
  (declare (ignorable start end translate width size))
  (apply 'xft:draw-text-line 
         drawable
         gcontext
         font
         sequence
         x y
         :draw-background-p t
         :allow-other-keys t
         keys))

