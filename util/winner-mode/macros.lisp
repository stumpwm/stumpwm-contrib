(in-package #:winner-mode)

(defmacro check-ids (group-number &body ids)
  `(progn
     ,@(loop for id in ids
          collect `(unless (gethash ,group-number ,id)
                     (setf (gethash ,group-number ,id) 0)))))
