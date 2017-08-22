(in-package #:searchengines)


(defvar *search-browser-executable* nil
  "Browser to use while performing searches")
(defvar *search-browser-params* nil
  "Additional executable parameters for searching browser")

(defun preprocess-and-search (url search &optional (raw-search nil) (raise-browser t))
  (unless *search-browser-executable*
    (message-no-timeout "searchengines:*search-browser-executable* is nil, set it first."))
  (let* ((search-processed (if raw-search
                               search
                               (url-encode search :utf-8)))
         (uri (format nil url search-processed)))
    (run-shell-command
     (concatenate 'string
                  *search-browser-executable*
                  " " (format nil "~{~A~^ ~}" *search-browser-params*)
                  " \"" uri "\""))
    (when raise-browser
      (funcall (intern (string-upcase *search-browser-executable*))))))

(defmacro make-searchengine-prompt (name caption url docstring
                                    &key (map nil) (key nil) (binded t))
  `(progn
     (if ,map
         (define-key ,map (kbd ,key) nil))
     (defcommand ,(intern (string-upcase name)) (search)
         ((:string ,(concatenate 'string "Search in " caption " for: ")))
       ,docstring
       (when search
         (check-type search string)
         (preprocess-and-search ,url search)))
     ,(when (and map key binded)
            `(define-key ,map (kbd ,key) ,(string-downcase (string name))))))

(defmacro make-searchengine-selection (name url docstring
                                       &key (map nil) (key nil) (binded t))
  `(progn
     (if ,map
         (define-key ,map (kbd ,key) nil))
     (defcommand ,(intern (string-upcase name)) () ()
       ,docstring
       (preprocess-and-search ,url (get-x-selection)))
     ,(when (and map key binded)
            `(define-key ,map (kbd ,key) ,(string-downcase (string name))))))

(defmacro make-searchengine-augmented (name caption url docstring
                                       &key (map nil) (key nil) (binded t))
  `(progn
     (if ,map
         (define-key ,map (kbd ,key) nil))
     (defcommand ,(intern (string-upcase name)) (augmentation)
         ((:string ,(concatenate 'string "Augment " caption " search: ")))
       ,docstring
       (when augmentation
         (check-type augmentation string)
         (preprocess-and-search ,url (concatenate
                                      'string
                                      (url-encode augmentation :utf-8) " "
                                      (url-encode (get-x-selection) :utf-8))
                                t)))
     ,(when (and map key binded)
            `(define-key ,map (kbd ,key) ,(string-downcase (string name))))))
