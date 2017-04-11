;;;; which-key.lisp

(in-package #:which-key)

(export '(get-kmaps-at-key-seq key-press-hook replace-hook))

;;; "which-key" goes here. Hacks and glory await!

(defun get-kmaps-at-key (kmaps key)
  (dereference-kmaps
   (reduce
    (lambda (result map)
      (let* ((binding (find key (kmap-bindings map)
                            :key 'binding-key :test 'equalp))
             (command (when binding (binding-command binding))))
        (if command
            (setf result (cons command result))
            result)))
    kmaps
    :initial-value '())))

(defun get-kmaps-at-key-seq (kmaps key-seq)
  "get a list of kmaps that are activated when pressing KEY-SEQ when
KMAPS are enabled"
  (if (= 1 (length key-seq))
      (get-kmaps-at-key kmaps (first key-seq))
      (get-kmaps-at-key-seq (get-kmaps-at-key kmaps (first key-seq))
                            (rest key-seq))))

(defun key-press-hook (key key-seq cmd)
  "which-key replacement for *key-press-hook*"
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((maps (get-kmaps-at-key-seq (dereference-kmaps (top-maps))
                                        (reverse key-seq))))
      (when (remove-if-not 'kmap-p maps)
        (apply 'display-bindings-for-keymaps (reverse (cdr key-seq)) maps)))))

(defmacro replace-hook (hook fn)
  "Easily replace a hook with a new function"
  `(remove-hook ,hook ,fn)
  `(add-hook ,hook ,fn))

;; Example usage:
;; (replace-hook *key-press-hook* 'key-press-hook)
