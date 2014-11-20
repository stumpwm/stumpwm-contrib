(in-package #:surfraw)

(defmacro auto-define-surfraw-commands-from-elvis-list ()
  (let ((commands nil))
    (dolist (elvi (surfraw-elvis-list))
      (let ((key (first elvi))
            (description (second elvi)))
        (push `(defcommand ,(intern (concat "sr-" key)) (search)
                 ((:string ,(concat description ": ")))
                 ,description
                 (surfraw ,key search))
              commands)
        (push `(defcommand ,(intern (concat "sr-sel-" key)) () ()
                 (surfraw ,key (get-x-selection)))
              commands)))
    (cons 'progn (reverse commands))))

(auto-define-surfraw-commands-from-elvis-list)
