(defpackage #:pass
  (:use #:cl)
  (:export *password-store*))

(in-package #:pass)

(defvar *password-store* (merge-pathnames #p".password-store/" (user-homedir-pathname)))

(defun pass-entries ()
  (let ((home-ns-len (length (namestring *password-store*))))
    (mapcar
     (lambda (entry)
       (let ((entry-ns (namestring entry)))
         (subseq entry-ns home-ns-len (- (length entry-ns) 4))))
     (directory (make-pathname :directory `(,@(pathname-directory *password-store*)
                                              :wild-inferiors)
                               :name :wild
                               :type "gpg")))))

(stumpwm:defcommand pass-copy () ()
  "Put a password into the clipboard."
  (let ((entry (stumpwm:completing-read (stumpwm:current-screen)
                                        "entry: "
                                        (pass-entries)
                                        :initial-input ""
                                        :require-match t)))
    (when entry
      (stumpwm:run-shell-command (format nil "pass -c ~a" entry)))))

(stumpwm:defcommand pass-copy-menu () ()
  "Select a password entry from a menu and copy the password into the clipboard."
  (let ((entry (stumpwm:select-from-menu
                (stumpwm:current-screen)
                (mapcar 'list (pass-entries))
                "Copy password to clipboard: ")))
    (when entry
      (stumpwm:run-shell-command (format nil "pass -c ~a" (car entry))))))

(stumpwm:defcommand pass-generate () ()
  "Generate a password and put it into the clipboard"
  (let ((entry-name (stumpwm:read-one-line (stumpwm:current-screen)
                                           "entry name: ")))
    (when entry-name
      (stumpwm:run-shell-command (format nil "pass generate -c ~a" entry-name)))))
