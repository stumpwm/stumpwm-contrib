(in-package #:clim-mode-line)

(defmacro define-pointer-gesture (name gesture-spec)
  `(define-gesture-name ,name :pointer-button-press ,gesture-spec))

;; pointer button
(define-pointer-gesture :left-click (:left))
(define-pointer-gesture :right-click (:right))

;; Pointer button with one modifier (shift cannot be used)
(define-pointer-gesture :left-click-control (:left :control))
(define-pointer-gesture :left-click-meta (:left :meta))
(define-pointer-gesture :left-click-super (:left :super))

(define-pointer-gesture :right-click-control (:right :control))
(define-pointer-gesture :right-click-meta (:right :meta))
(define-pointer-gesture :right-click-super (:right :super))


;; Pointer button with two modifiers
(define-pointer-gesture :left-click-control-meta (:left :control :meta))
(define-pointer-gesture :left-click-control-super (:left :control :super))
(define-pointer-gesture :left-click-meta-super (:left :meta :super))

(define-pointer-gesture :right-click-control-meta (:right :control :meta))
(define-pointer-gesture :right-click-control-super (:right :control :super))
(define-pointer-gesture :right-click-meta-super (:right :meta :super))

;; Pointer button with three modifiers
(define-pointer-gesture :right-click-control-meta-super
    (:right :control :meta :super))
(define-pointer-gesture :left-click-control-meta-super
    (:left :control :meta :super))

