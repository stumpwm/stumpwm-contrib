(in-package #:winner-mode)

(defun current-group-number ()
  (slot-value (stumpwm:current-group) 'number))

(defun dump-name (group-number id)
  (merge-pathnames
   (pathname (format nil "stumpwm-winner-mode-~d-~10,'0d" group-number id))
   *tmp-folder*))

(defun dump-group-to-file (&rest args)
  (declare (ignore args))
  (let* ((group-number (current-group-number)))
    (check-ids group-number *current-ids* *max-ids*)
    (stumpwm:dump-group-to-file
     (dump-name group-number (incf (gethash group-number *current-ids*))))
    (when (> (gethash group-number *current-ids*)
             (gethash group-number *max-ids*))
      (setf (gethash group-number *max-ids*)
            (gethash group-number *current-ids*)))))
