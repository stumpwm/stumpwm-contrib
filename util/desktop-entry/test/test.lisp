(require :desktop-entry)
(require :fiveam)
(in-package #:desktop-entry)
(defconstant *desktop-entry-pathnames*
  '(#P"google-chrome.desktop"
    #P"emacs.desktop"
    #P"xterm.desktop"
    #P"i3.desktop"
    #P"libfm-pref-apps.desktop"
    #P"firefox.desktop"))
(defconstant *true-value-plist*
  '((:name "Google Chrome"
     :entry-type "Application"
     :exec "/usr/bin/google-chrome-stable %U"
     :path nil
     :categories ("Network" "WebBrowser")
     :no-display nil
     :only-show-in nil
     :terminal nil)
    (:name "GNU Emacs"
     :entry-type "Application"
     :exec "/usr/bin/emacs %F"
     :path nil
     :categories ("Development" "TextEditor")
     :no-display nil
     :only-show-in nil
     :terminal nil)
    (:name "XTerm"
     :entry-type "Application"
     :exec "xterm"
     :path "/usr/local/bin"
     :categories ("System" "TerminalEmulator")
     :no-display nil
     :only-show-in nil
     :terminal t)
    (:name "i3"
     :entry-type "Application"
     :exec "i3"
     :path nil
     :categories nil
     :no-display t
     :only-show-in nil
     :terminal nil)
    (:name "Preferred Applications"
     :entry-type "Application"
     :exec "libfm-pref-apps"
     :path nil
     :categories ("Settings" "DesktopSettings" "X-LXDE-Settings" "GTK")
     :no-display nil
     :only-show-in ("LXDE")
     :terminal nil)
    (:name "Mozilla Firefox"
     :entry-type "Application"
     :exec "firefox %u"
     :path nil
     :categories ("Network" "WebBrowser")
     :no-display nil
     :only-show-in nil
     :terminal nil)))
(defconstant *true-value-entry*
  (loop for item in *true-value-plist*
     collect (make-desktop-entry item)))

(block test-desktop-entry
  (fiveam:def-suite test-desktop-entry-suite)
  (fiveam:in-suite test-desktop-entry-suite)

  (fiveam:test
   init-desktop-entry
   (fiveam:is (equalp (load-desktop-file "google-chrome.desktop")
                      (first *true-value-plist*)))
   (loop for index from 0 below (length *desktop-entry-pathnames*)
      do (fiveam:is (equalp (load-desktop-file
                             (nth index *desktop-entry-pathnames*))
                            (nth index *true-value-plist*))))
   (loop for index from 0 below (length *desktop-entry-pathnames*)
      do (fiveam:is (desktop-entry-equal
                     (make-desktop-entry (nth index *desktop-entry-pathnames*))
                     (nth index *true-value-entry*)))))

  (fiveam:test
   test-add-to-entry-list-1
   (let* ((entry-list nil)
          (entry-list (add-to-entry-list
                       entry-list
                       (make-desktop-entry #P"google-chrome.desktop")))
          (entry-list-2 (add-to-entry-list
                         entry-list
                         (make-desktop-entry #P"emacs.desktop")))
          (entry-list-3 (add-to-entry-list
                         entry-list-2
                         (make-desktop-entry #P"google-chrome.desktop"))))
     (fiveam:is (and (= 1 (length entry-list))
                     (eq 'desktop-entry
                         (type-of (first entry-list)))
                     (desktop-entry-equal
                      (first *true-value-entry*)
                      (first entry-list))))
     (fiveam:is (and (= 2 (length entry-list-2))
                     (desktop-entry-equal
                      (second *true-value-entry*)
                      (second entry-list-2))))
     (fiveam:is (= 2 (length entry-list-3)))))

  (fiveam:test
   test-add-to-entry-list-2
   (let ((entry-list nil))
     (loop for index from 0 below (length *desktop-entry-pathnames*)
        do (setf entry-list (add-to-entry-list
                             entry-list
                             (nth index *desktop-entry-pathnames*)))
          (fiveam:is (= (+ index 1) (length entry-list)))
          (fiveam:is (desktop-entry-equal
                      (nth index entry-list)
                      (nth index *true-value-entry*)))
        do (setf entry-list (add-to-entry-list
                             entry-list
                             (make-desktop-entry
                              (nth index *desktop-entry-pathnames*))))
          (fiveam:is (= (+ index 1) (length entry-list)))
          (fiveam:is (desktop-entry-equal
                      (nth index entry-list)
                      (nth index *true-value-entry*))))))

  (fiveam:test
   test-add-category
   (let ((entry (make-desktop-entry
                 (first *desktop-entry-pathnames*))))
     (fiveam:is (equal '("Network" "WebBrowser")
                       (categories entry)))
     (add-category entry "Test")
     (fiveam:is (equal '("Network" "WebBrowser" "Test")
                       (categories entry)))
     (add-category entry "Test")
     (fiveam:is (equal '("Network" "WebBrowser" "Test")
                       (categories entry)))
     (add-category entry "Network")
     (fiveam:is (equal '("Network" "WebBrowser" "Test")
                       (categories entry)))))

  (fiveam:def-suite test-entry-list-suite)
  (fiveam:in-suite test-entry-list-suite)


  (fiveam:test
   test-find-categories
   (fiveam:is (equal '("Network" "WebBrowser"
                       "Development" "TextEditor"
                       "System" "TerminalEmulator"
                       "Settings" "DesktopSettings"
                       "X-LXDE-Settings" "GTK")
                     (find-categories *true-value-entry*)))
   (fiveam:is (equal '("Network" "WebBrowser"
                       "Development" "TextEditor"
                       "System" "TerminalEmulator")
                     (find-categories
                      *true-value-entry*
                      :modify
                      #'(lambda (entry)
                          (if (only-show-in entry)
                              nil
                              (categories entry)))))))
  (fiveam:test
   test-entry-in-categories-p
   (let ((entry-1 (first *true-value-entry*))
         (entry-2 (fifth *true-value-entry*)))
     (fiveam:is (entry-in-categories-p entry-1 '("Network")))
     (fiveam:is (not (entry-in-categories-p entry-1 '("Test"))))
     (fiveam:is (not (entry-in-categories-p entry-1 '("Network" "Test"))))
     (fiveam:is (entry-in-categories-p entry-1 '("Network" "WebBrowser")))
     (fiveam:is (entry-in-categories-p entry-2 '("Settings" "DesktopSettings"
                                                 "GTK")))
     (fiveam:is (not (entry-in-categories-p entry-2 '("Settings" "DesktopSettings"
                                                      "GTK" "Test"))))))

  (fiveam:test
   test-find-entries
   (fiveam:is (not (find-entries *true-value-entry*)))
   (fiveam:is (= 2
                 (length
                  (find-entries
                   *true-value-entry*
                   :test #'(lambda (entry)
                             (entry-in-categories-p entry '("Network")))))))
   (fiveam:is (every
               #'(lambda (a b) (desktop-entry-equal a b))
               (find-entries
                *true-value-entry*
                :test #'(lambda (entry)
                          (entry-in-categories-p entry '("Network"))))
               (list (first *true-value-entry*) (nth 5 *true-value-entry*)))))

  (fiveam:test
   test-group-entries
   (flet ((compare-groups (a b)
            (fiveam:is (= (length a) (length b)))
            (every #'(lambda (group1 group2)
                       (fiveam:is (string= (car group1) (car group2)))
                       (fiveam:is (= (length (cdr group1))
                                     (length (cdr group2))))
                       (every #'(lambda (x y)
                                  (fiveam:is (desktop-entry-equal x y)))
                              (cdr group1)
                              (cdr group2)))
                   a b)))
     (compare-groups (list (list "Network" (first *true-value-entry*)))
                     (list (list "Network" (first *true-value-entry*))))
     (compare-groups (list (list "Network"
                                 (first *true-value-entry*)
                                 (nth 5 *true-value-entry*))
                           (list "WebBrowser"
                                 (first *true-value-entry*)
                                 (nth 5 *true-value-entry*))
                           (cons nil
                                 (loop for index in '(1 2 3 4)
                                    collect (nth index *true-value-entry*))))
                     (group-entries *true-value-entry*
                                    :categories
                                    '("Network" "WebBrowser")))
     (compare-groups
      (list (list "DesktopSettings"
                  (fifth *true-value-entry*))
            (list "Development"
                  (second *true-value-entry*))
            (cons nil
                  (loop for index in '(0 2 3 5)
                     collect (nth index *true-value-entry*))))
      (group-entries *true-value-entry*
                     :categories
                     '("DesktopSettings" nil "Development")))
     (compare-groups
      (list (list nil
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*)))
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))))
     (compare-groups
      (list (list "Network"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            (list "WebBrowser"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            nil)
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))
                     :categories
                     '("Network" "WebBrowser")
                     :min-count nil))
     (compare-groups
      (list (list "Network"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            (list "WebBrowser"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            nil)
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))
                     :categories
                     '("Network" "WebBrowser")
                     :min-count 0))
     (compare-groups
      (list (list "Network"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            (list "WebBrowser"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            nil)
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))
                     :categories
                     '("Network" "WebBrowser")
                     :min-count -1))
     (compare-groups
      (list (list "Network"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            (list "WebBrowser"
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*))
            nil)
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))
                     :categories
                     '("Network" "WebBrowser")
                     :min-count 1))
     (compare-groups
      (list (list nil
                  (first *true-value-entry*)
                  (nth 5 *true-value-entry*)))
      (group-entries (list (first *true-value-entry*)
                           (nth 5 *true-value-entry*))
                     :categories
                     '("Network" "WebBrowser")
                     :min-count 3))))

  (fiveam:run! 'test-desktop-entry-suite)
  (fiveam:run! 'test-entry-list-suite)

  ;; (init-entry-list)
  ;; (format t "menu ~S~%" (build-menu '("AudioVideo") :min-entry-in-category 3))

  ;; (format t "menu ~S~%" (build-menu '("Office") :min-entry-in-category 5))
  (return-from test-desktop-entry)
  )
