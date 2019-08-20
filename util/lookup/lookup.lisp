;;;; lookup.lisp

(in-package #:lookup)

;;; "lookup" goes here. Hacks and glory await!

;; Dictionary/search engine lookup module for StumpWM.
;;
;; Copyright (C) 2019 Wojciech S. Gac
;;
;; Maintainer: Wojciech S. Gac
;;

;;; Code:

;; Dictionary Lookup
(defparameter *dictionaries*
  '(:en "https://www.thefreedictionary.com/~a"
    :ja "https://jisho.org/search/~a"
    :ru "http://en.pons.com/translate?q=~a&l=enru"
    :pl "https://sjp.pwn.pl/szukaj/~a.html"
    :de "https://dict.tu-chemnitz.de/dings.cgi?service=deen&opterrors=0&optpro=0&query=~a&iservice="
    :es "https://www.spanishdict.com/translate/~a")
  "List of dictionaries available to the service. Right now there is
  one dictionary per language. This may change in the future.")

(defparameter *sticky-language-selection* t
  "When set to non-NIL, the system will by default suggest the
  language used for the previous lookup.")

(defparameter *last-language-selected* nil
  "Stores the last language selected for lookup.")

(defun %dictionary-lookup (lang &key (dictionaries *dictionaries*))
  "Given a language LANG, obtain a lookup phrase from the user,
suggesting current X selection."
  (let ((url-template (getf dictionaries (make-keyword (string-upcase lang)))))
    (unless url-template
      (error "Unknown lookup language: ~a" lang))
    (let* ((phrase (read-one-line (current-screen)
				  (format nil "Lookup [~a]: " lang)
				  :initial-input (get-x-selection)))
	   (url (format nil url-template
			(quri::url-encode (or phrase ; Non-local exit if empty
					      (return-from %dictionary-lookup nil))))))
      (run-shell-command (format nil "xdg-open '~a'" url)))))


(defcommand dictionary-lookup () ()
  (let ((lang (completing-read (current-screen) "Language: "
			       (loop for (k v) on *dictionaries* by #'cddr
				     collect (symbol-name k)
				     collect (string-downcase
					      (symbol-name k)))
			       :require-match t
                               :initial-input (when *sticky-language-selection*
                                                *last-language-selected*))))
    (when *sticky-language-selection*
      (setf *last-language-selected* lang))
    (%dictionary-lookup (or lang
			    (return-from dictionary-lookup)))))

;; Search Engine Lookup
(defparameter *search-engine*
  "https://www.google.com/search?q=~a")

(defun %search-lookup ()
  "Interactively obtain a lookup phrase. By default suggest current X
selection. Pass the phrase to the search engine and open result in the
browser."
  (let* ((phrase (read-one-line (current-screen) "Google Lookup: "
				:initial-input (get-x-selection)))
	 (url (format nil *search-engine*
		      (quri::url-encode (or phrase
					    (return-from %search-lookup nil))))))
    (run-shell-command (format nil "xdg-open ~a" url))))

(defcommand search-lookup () ()
  (%search-lookup))

;; Key Bindings
(define-key *top-map* (kbd "s-d") "dictionary-lookup")
(define-key *top-map* (kbd "s-g") "search-lookup")
