(in-package #:surfraw)

(defmacro auto-define-surfraw-commands-from-elvis-list ()
  (let ((commands nil))
    (dolist (elvi (surfraw-elvis-list))
      (let ((key (first elvi))
            (description (second elvi)))
        (push `(defcommand ,(format-symbol t "~@:(sr-~a~)" key) (search)
                 ((:string ,(concat description ": ")))
                 ,description
                 (surfraw ,key search))
              commands)
        (push `(defcommand ,(format-symbol t "~@:(sr-sel-~a~)" key) () ()
                 (surfraw ,key (get-x-selection)))
              commands)))
    (cons 'progn (reverse commands))))

(auto-define-surfraw-commands-from-elvis-list)
