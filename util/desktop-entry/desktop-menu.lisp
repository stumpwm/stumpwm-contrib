;;;; desktop-menu.lisp

(in-package #:desktop-entry)

(defvar *main-categories*
  (list
   "AudioVideo"
   "Audio"
   "Video"
   "Development"
   "Education"
   "Game"
   "Graphics"
   "Network"
   "Office"
   "Settings"
   "System"
   "Utility"))
(defvar *favorite-category* "Favorite")
(defvar *entry-paths*
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
(defvar *entry-list* '())
(defvar *favorite-list* '())

(defgeneric add-favorite-entry (entry)
  (:documentation "add entry as favorite"))

(defmethod add-favorite-entry ((entry desktop-entry))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry pathname))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry-name string))
  (let ((entry-index (position
                      entry-name *entry-list*
                      :test #'(lambda (name entry)
                                (string= name (name entry))))))
    (when entry-index
      (add-favorite-entry (nth entry-index *entry-list*)))))


(defun init-entry-list (&optional (entry-paths *entry-paths*))
  (setf *entry-list* nil)
  (dolist (entry-path entry-paths)
    (dolist (entry-file (list-entry-files entry-path))
      (setf *entry-list*
            (add-to-entry-list *entry-list* entry-file)))))

(defun build-menu (categories)
  (let* ((min-entries-in-category (if (not categories) nil 5))
         (favorite-p (if (string= (first categories) *favorite-category*)
                         t
                         nil))
         (entry-list
          (find-entries
           (if favorite-p *favorite-list* *entry-list*)
           :test #'(lambda (entry)
                     (and (not (no-display entry))
                          (not (only-show-in entry))
                          (string= "Application" (entry-type entry))
                          (entry-in-categories-p
                           entry
                           (if favorite-p
                               (cdr categories)
                               categories))))))
         (menu
          (group-entries
           entry-list
           :categories
           (if categories
               (loop for item in (find-categories entry-list)
                  when (not (member item categories :test #'string=))
                  collect item)
               *main-categories*)
           :min-count min-entries-in-category))
         (menu (loop for item in menu
                  when (not (car item))
                  append (loop for entry in (cdr item)
                            collect (cons (name entry)
                                          entry))
                  else
                  collect (cons (car item) (car item))))
         (menu (sort-menu menu))
         (menu (if categories
                   menu
                   (cons
                    (cons *favorite-category*
                          *favorite-category*)
                    menu))))
    menu))

(defun sort-menu (menu)
  (sort menu
        #'(lambda (x y)
            (cond
              ((and (typep x 'desktop-entry)
                    (stringp y))
               nil)
              ((and (stringp x)
                    (typep y 'desktop-entry))
               T)
              ((and (stringp x)
                    (stringp y))
               (string-lessp x y))
              ((and (typep x 'desktop-entry)
                    (typep y 'desktop-entry))
               (string-lessp (name x) (name y)))
              (T nil))) :key #'cdr))


(stumpwm:defcommand show-desktop-menu ()
  ()
  "show the application menu"
  (let ((categories nil))
    (loop
       (let* ((menu (build-menu categories))
              (menu (if categories
                        (append menu (list (cons ".." :up)
                                           (cons "...." nil)))
                        (append menu (list (cons ".." nil)))))
              (menu (loop for item in menu
                       collect
                         (if (stringp (cdr item))
                             (cons (concatenate 'string (car item) " >>")
                                   (cdr item))
                             item)))
              (menu (loop for item in menu
                       collect (cons
                                (concatenate 'string "^[^6*^b" (car item) "^]")
                                (cdr item))))
              (item (handler-case
                        (cdr (stumpwm:select-from-menu
                              (stumpwm:current-screen)
                              menu
                              (format nil "/~{~A/~}:" (reverse categories))))
                      (error (condition) nil))))
         (cond
           ((not item) (return))
           ((stringp item) (push item categories))
           ((typep item 'desktop-entry)
            (stumpwm:run-shell-command (command-line item))
            (return))
           ((eq item :up)
            (pop categories)))))))
