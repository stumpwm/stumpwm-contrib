(in-package #:winner-mode)

(stumpwm:defcommand winner-undo () ()
  "Go back to the last frame setup"
  (let* ((group-number (current-group-number)))
    (check-ids group-number *current-ids*)
    (if (> (gethash group-number *current-ids*) 1)
        (stumpwm:restore-from-file
         (dump-name
          group-number
          (decf (gethash group-number *current-ids*))))
        (error "No previous frame setup"))))

(stumpwm:defcommand winner-redo () ()
  "Go forward to the next frame setup"
  (let* ((group-number (current-group-number)))
    (check-ids group-number *current-ids* *max-ids*)
    (if (= (gethash group-number *max-ids*) (gethash group-number *current-ids*))
        (error "No next frame setup")
        (stumpwm:restore-from-file
         (dump-name
          group-number
          (incf (gethash group-number *current-ids*)))))))

(stumpwm:add-hook stumpwm:*quit-hook* (lambda ()
                                        (mapcar #'delete-file
                                                (directory (merge-pathnames
                                                            #p"stumpwm-winner-mode-*"
                                                            *tmp-folder*)))))
