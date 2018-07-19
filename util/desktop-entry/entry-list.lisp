;;;; entry-list.lisp

(in-package #:desktop-entry)

(defgeneric add-to-entry-list (entry-list entry)
  (:documentation "add an entry to entry-list"))

(defmethod add-to-entry-list ((entry-list list) (entry desktop-entry))
  (if (member entry entry-list :test #'desktop-entry-equalp)
      entry-list
      (append entry-list (list entry))))

(defmethod add-to-entry-list ((entry-list list) (entry pathname))
  (let ((entry (make-desktop-entry entry)))
    (if entry (add-to-entry-list entry-list entry)
        entry-list)))

(defun find-entries (entry-list &optional &key (test #'(lambda (entry) nil)))
  (loop for entry in entry-list
     when (funcall test entry)
     collect entry))

(defun find-categories (entry-list
                        &optional
                        &key
                          (modify #'(lambda (entry) (categories entry))))
  (let ((category-list nil))
    (dolist (entry entry-list)
      (dolist (category (funcall modify entry))
        (when (not (member category category-list :test #'string=))
          (setf category-list (nconc category-list (list category))))))
    category-list))

(defun group-entries (entry-list &optional &key categories
                                             (min-count 0))
  (let* ((groups
          (loop for category in categories
             when (not (eq category nil))
             collect
               (cons category
                     (loop for entry in entry-list
                        when (entry-in-categories-p entry (list category))
                        collect entry))))
         (groups
          (loop for item in groups
             when (cond ((<= (length (cdr item)) 0) nil)
                        ((not min-count) t)
                        ((<= min-count 0) t)
                        ((<= min-count (length (cdr item))) t)
                        (t nil))
             collect item))
         (others
          (loop for entry in entry-list
             when (not
                   (member entry groups
                           :test
                           #'(lambda (a b)
                               (member a (cdr b)
                                       :test #'eq))))
             collect entry)))
    (append groups (list (cons nil others)))))
