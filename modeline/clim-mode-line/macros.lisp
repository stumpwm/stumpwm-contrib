(in-package #:clim-mode-line)

;;; This file contains general macros that dont have a clear place where they
;;; should reside

(defmacro do-list-with-interspersed-element
    ((var list &body interspersed-forms) &body body)
  (alexandria:with-gensyms (initial rest hold cont tmp)
    `(flet ((,cont (,var)
              ,@body))
       (let* ((,hold ,list)
              (,initial (car ,hold))
              (,rest (cdr ,hold)))
         (when ,initial
           (,cont ,initial))
         (dolist (,tmp ,rest)
           ,@interspersed-forms
           (,cont ,tmp))))))

(defmacro with-undrawn-output-record ((stream) &body body)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (with-output-recording-options (,s :draw nil :record t)
         (with-new-output-record (,s)
           ,@body)))))

(defmacro with-output-record-bounds ((x y width height) record &body body)
  (alexandria:with-gensyms (rec)
    `(let ((,rec ,record))
       (multiple-value-bind (,x ,y) (output-record-position ,rec)
         (declare (ignorable ,x ,y))
         (multiple-value-bind (,width ,height) (bounding-rectangle-size ,rec)
           (declare (ignorable ,width ,height))
           ,@body)))))

(defmacro with-right-alignment ((frame pane) &body body)
  (alexandria:with-gensyms (stream width record)
    `(let* ((,stream ,pane)
            (,record
              (with-output-recording-options (,stream :draw nil :record t)
                (with-new-output-record (,stream)
                  ,@body)))
            (,width (mode-line-head-width ,frame)))
       (multiple-value-bind (x y) (output-record-position ,record)
         (declare (ignore x))
         (multiple-value-bind (w h) (bounding-rectangle-size ,record)
           (declare (ignore h))
           (setf (output-record-position ,record)
                 (values (- ,width w) y))))
       (tree-recompute-extent ,record)
       (replay ,record ,stream))))

(defmacro with-inverted-ink ((pane &rest drawing-options
                              &key (ink '+flipping-ink+) fg-ink bg-ink
                              &allow-other-keys)
                             &body body)
  (let ((p (gensym "PANE")))
    `(let ((,p ,pane))
       (surrounding-output-with-border (,p :ink ,(or bg-ink ink)
                                           :filled t
                                           :move-cursor nil)
         (with-drawing-options (,p :ink ,(or fg-ink ink) ,@drawing-options)
           ,@body)))))

(defmacro with-table ((pane &rest options) &body body)
  ;; Set up a table, and make sure we can communicate it to functions further down
  ;; the call stack.
  `(slim:with-table (,pane ,@options)
     (let ((*display-as-table* t)
           (*display-style* :table))
       ,@body)))

(defmacro with-table-row ((&rest options) &body body)
  ;; Defined because we want a consistent syntax. Plus then we can make our own
  ;; changes without anyone needing to rewrite anything (well, unless we add
  ;; required arguments)
  (declare (ignore options))
  `(slim:row ,@body))

(defmacro with-cell ((&optional (pane 'slim:*pane*) &rest options) &body body)
  (declare (ignore options))
  (alexandria:with-gensyms (cont)
    `(flet ((,cont ()
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       (case *display-style*
         ((:text) (funcall #',cont))
         ((:table) (formatting-cell (,pane :align-x *align-x*)
                     (funcall #',cont)))))))
