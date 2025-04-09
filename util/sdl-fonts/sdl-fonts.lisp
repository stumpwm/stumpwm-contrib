;;;; sdl-fonts.lisp

(in-package #:sdl-fonts)

(export '(load-font
          font-exists-p
          open-font
          close-font
          font-ascent
          font-descent
          font-height
          text-line-width
          draw-image-glyphs))

(define-foreign-library libsdl2
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2.so.0.2" "libSDL2")))

(define-foreign-library libsdl2-ttf
  (:unix (:or "libSDL2_ttf-2.0.so.0" "libSDL2_ttf")))

(use-foreign-library libsdl2)
(use-foreign-library libsdl2-ttf)

(defcstruct sdl-rect
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defcstruct sdl-surface
  (flags :uint32)
  (format :pointer)
  (w :int)
  (h :int)
  (pitch :int)
  (pixels :pointer)
  (userdata :pointer)
  (locked :int)
  (list-bitmap :pointer)
  (clip-rect (:struct sdl-rect))
  (map :pointer)
  (refcount :int))

(defcstruct sdl-color
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

; Using :compile-toplevel because the var is used in a declare type statement.
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *hinting-flags* '(:normal :light :mono :none :light-subpixel)))

(deftype hinting-mode () `(member ,@*hinting-flags*))

(defcfun "SDL_Init" :int (flags :long))
(defcfun "SDL_WasInit" :int (flags :long))
(defcfun "SDL_LockSurface" :void (surf :pointer))
(defcfun "SDL_UnlockSurface" :void (surf :pointer))
(defcfun "SDL_FreeSurface" :void (surf :pointer))
(defcfun "TTF_Init" :int)
(defcfun "TTF_WasInit" :int)
(defcfun "TTF_OpenFont" :pointer (file :string) (ptsize :int))
(defcfun "TTF_SizeUTF8" :int (font :pointer)
                             (text :string)
                             (w :pointer)
                             (h :pointer))
(defcfun "TTF_RenderUTF8_Blended" :pointer
                                  (font :pointer)
                                  (text :string)
                                  (fg (:struct sdl-color)))
(defcfun "TTF_FontAscent" :int (font :pointer))
(defcfun "TTF_FontDescent" :int (font :pointer))
(defcfun "TTF_FontHeight" :int (font :pointer))
(defcfun "TTF_SetFontSizeDPI" :int (font :pointer)
                              (ptsize :int)
                              (hdpi :unsigned-int)
                              (vdpi :unsigned-int))
(defcfun "TTF_SetFontHinting" :int (font :pointer) (hinting :int))
(defcfun "TTF_SetFontKerning" :int (font :pointer) (allowed :int))

(defclass font ()
  ((sdl2-ptr
     :initarg :sdl2-ptr
     :accessor sdl2-ptr)
   (size
     :initarg :size
     :accessor font-size)
   (hdpi
     :initform nil
     :accessor font-hdpi)
   (vdpi
     :initform nil
     :accessor font-vdpi)))

(defconstant SDL_INIT_VIDEO #x00000020)

(defun load-font (path size &key hinting (kerning t kerning-p))
  (declare (type (or null hinting-mode) hinting))
  (when (zerop (sdl-wasinit SDL_INIT_VIDEO))
    (sdl-init SDL_INIT_VIDEO))
  (when (zerop (ttf-wasinit))
    (ttf-init))
  (let ((font (make-instance 'font
                             :sdl2-ptr (ttf-openfont path size)
                             :size size)))
    (when hinting
      (ttf-setfonthinting (sdl2-ptr font)
                          (position hinting *hinting-flags*)))
    (when kerning-p
      (ttf-setfontkerning (sdl2-ptr font)
                          (if kerning 1 0)))
    font))

(defmethod font-exists-p ((font font))
  t)

(defmethod open-font (display (font font))
  font)

(defmethod close-font ((font font))
  t)

(defmethod font-ascent ((font font))
  (ttf-fontascent (sdl2-ptr font)))

(defmethod font-descent ((font font))
  (ttf-fontdescent (sdl2-ptr font)))

(defmethod font-height ((font font))
  (ttf-fontheight (sdl2-ptr font)))

(defun get-destination-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-surface)
      (setf (getf (xlib:drawable-plist drawable) :ttf-surface)
            (xlib:render-create-picture
              drawable
              :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
                                                                  :depth (xlib:drawable-depth drawable)))))))
(defun get-source-pixmap (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
      (setf (getf (xlib:drawable-plist drawable) :ttf-pen-surface)
            (xlib:create-pixmap
              :drawable drawable
              :depth (xlib:drawable-depth drawable)
              :width 1 :height 1))))

(defun get-source-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ttf-pen)
      (setf (getf (xlib:drawable-plist drawable) :ttf-pen)
            (xlib:render-create-picture
              (get-source-pixmap drawable)
              :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
                                                                  :depth (xlib:drawable-depth drawable)))
              :repeat :on))))

(defun display-alpha-picture-format (display)
  (or (getf (xlib:display-plist display) :ttf-alpha-format)
      (setf (getf (xlib:display-plist display) :ttf-alpha-format)
            (first
              (xlib:find-matching-picture-formats
                display
                :depth 8 :alpha 8 :red 0 :blue 0 :green 0)))))

(defun drawable-screen (drawable)
  (typecase drawable
    (xlib:drawable
      (dolist (screen (xlib:display-roots (xlib:drawable-display drawable)))
        (when (xlib:drawable-equal (xlib:screen-root screen) (xlib:drawable-root drawable))
          (return screen))))
    (xlib:screen drawable)
    (t nil)))

(defun screen-default-dpi (screen)
  "Returns default dpi for @var{screen}. pixel width * 25.4/millimeters width"
  (values (floor (* (xlib:screen-width screen) 25.4)
                 (xlib:screen-width-in-millimeters screen))
          (floor (* (xlib:screen-height screen) 25.4)
                 (xlib:screen-height-in-millimeters screen))))

(defun update-dpi (font drawable)
  (multiple-value-bind (hdpi vdpi) (screen-default-dpi (drawable-screen drawable))
    (when (or (not (eq (font-hdpi font) hdpi))
              (not (eq (font-vdpi font) vdpi)))
      (ttf-setfontsizedpi (sdl2-ptr font) (font-size font) hdpi vdpi)
      (setf (font-hdpi font) hdpi)
      (setf (font-vdpi font) vdpi))))

(defmethod text-line-width ((font font) text &rest keys &key (start 0) end translate)
  (declare (ignorable keys start end translate))
  (with-foreign-object (sizes :int 2)
                       (ttf-sizeutf8
                         (sdl2-ptr font)
                         text
                         sizes
                         (inc-pointer sizes (foreign-type-size :int)))
                       (mem-ref sizes :int 0)))

(defmethod draw-image-glyphs (drawable
                               gcontext
                               (font font)
                               x y
                               text &rest keys
                               &key (start 0) end translate width size)
  (declare (ignorable keys start end translate width size))
  (when (string= text "")
    (return-from draw-image-glyphs))
  ; Update the DPI in case it has changed (i.e. rendering to another screen).
  (update-dpi font drawable)
  ; This is ugly code but the idea is that we don't really know anything about
  ; the color format etc. of the thing we are drawing on (i.e. the screen). So
  ; we use the alpha channel of the rendered glyphs to stencil out a rectangle
  ; in the foreground color onto a rectangle of the background color. This is
  ; the only way to properly handle all possible cases.
  (let* ((surf (ttf-renderutf8-blended
                 (sdl2-ptr font)
                 text
                 '(r 0 b 0 g 0 a 0)))
         (width (foreign-slot-value surf '(:struct sdl-surface) 'w))
         (height (foreign-slot-value surf '(:struct sdl-surface) 'h))
         (pitch (foreign-slot-value surf '(:struct sdl-surface) 'pitch))
         (data (make-array (list height width) :element-type '(unsigned-byte 8))))
    (sdl-locksurface surf)
    (let ((pixels (foreign-slot-value surf '(:struct sdl-surface) 'pixels)))
      (dotimes (jj height)
        (dotimes (ii width)
          (let ((alpha (mem-ref pixels :uint8 (+ (* jj pitch) (* ii 4) 3))))
            (setf (aref data jj ii) alpha)))))
    (sdl-unlocksurface surf)
    (sdl-freesurface surf)
    (let* ((display (xlib:drawable-display drawable))
           (image (xlib:create-image
                    :depth 8
                    :width width
                    :height height
                    :data data))
           (alpha-pixmap (xlib:create-pixmap :width width
                                             :height height
                                             :depth 8
                                             :drawable drawable))
           (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
           (alpha-pic
             (progn
               (xlib:put-image alpha-pixmap alpha-gc image :x 0 :y 0)
               (xlib:render-create-picture alpha-pixmap
                                           :format (display-alpha-picture-format display))))
           (src-pic (get-source-picture drawable))
           (dst-pic (get-destination-picture drawable)))
      (xlib:free-gcontext alpha-gc)
      ; Paint the source & destination surfaces in the foreground and
      ; background colors of the context.
      (xlib:draw-point (get-source-pixmap drawable) gcontext 0 0)
      (let ((fg (xlib:gcontext-foreground gcontext)))
        (setf (xlib:gcontext-foreground gcontext) (xlib:gcontext-background gcontext))
        (xlib:draw-rectangle drawable gcontext x (- y (font-ascent font)) width height t)
        (setf (xlib:gcontext-foreground gcontext) fg))
      (xlib:render-composite :over
                             src-pic
                             alpha-pic
                             dst-pic
                             0
                             0
                             0
                             0
                             x
                             (- y (font-ascent font))
                             width
                             height)
      (xlib:render-free-picture alpha-pic)
      (xlib:free-pixmap alpha-pixmap))))
