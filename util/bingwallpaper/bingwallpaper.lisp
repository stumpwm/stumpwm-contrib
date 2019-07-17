
(in-package :bingwallpaper)

(defparameter *cache-dir* (merge-pathnames (concatenate 'string (or (stumpwm:getenv "XDG_CACHE_HOME") (concatenate 'string (stumpwm:getenv "HOME") "//.cache/")) "/stumpwm/")))

(defparameter *wallpaper-filename* (merge-pathnames *cache-dir* "bingwallpaper.jpg"))
(defparameter *wpinfo-filename* (merge-pathnames *cache-dir* "wallpaperinfo.txt"))

(defvar *wallpaper-info* "Unknown")
(defparameter *wpinfo-fmt* "^5*~A%^2*~A")
(defparameter *split-copyright* "~%\\1")
(defparameter *feh-executable* "/usr/bin/feh")

(defparameter *bing-url-fmt* "https://www.bing.com/HPImageArchive.aspx?format=~A&idx=~A&n=1&mkt=~A")

(defun get-bing-url (&key (type "js") (index 0) (mkt "en-US"))
  "Gets the URL for wallpaper information.
     TYPE -- the format of information returned, can be \"xml\" for XML or \"js\" for JSON.
     INDEX -- get the wallpaper from this many days ago, 0 (the default) is today's.
     MKT -- the locale for the image caption."
  (format nil *bing-url-fmt* type index mkt))

(defun get-bing-img-info (&key (index 0))
  "Gets an ALIST of information about the Bing wallpaper image from INDEX days ago."
  (let* ((jsonbin (nth-value 0 (drakma:http-request (get-bing-url :index index))))
		 (jsonstr (flexi-streams:octets-to-string jsonbin :external-format :utf-8))
		 (imginfo (second (assoc :images (cl-json:decode-json-from-string jsonstr)))))
	(setf (assoc-value imginfo :copyright)
		  (cl-ppcre:regex-replace " (\\(©.*)" (assoc-value imginfo :copyright)
								  *split-copyright*))
	imginfo))

(defun set-bing-wallpaper (&key (index 0))
  "Sets the Bing wallpaper from INDEX days ago."
  (let* ((imginfo (get-bing-img-info :index index))
		 (imgurl (concatenate 'string "https://www.bing.com"
							  (assoc-value imginfo :url)))
		 (imgdata (nth-value 0 (drakma:http-request imgurl)))
		 proc procstream)
	(setf *wallpaper-info* (format nil *wpinfo-fmt*
								   (assoc-value imginfo :title)
								   (assoc-value imginfo :copyright)))
	(unwind-protect
		 (progn
		   (setf proc (sb-ext:run-program *feh-executable* '("--bg-scale" "-")
										  :wait nil :input :stream))
		   (setf procstream (sb-ext:process-input proc))
		   (write-sequence imgdata procstream)
		   (finish-output procstream)
		   (close procstream)
		   (sb-ext:process-wait proc)
		   (stumpwm:message *wallpaper-info*))
	  (sb-ext:process-close proc))
	(stumpwm:message *wallpaper-info*)))

(defun update-cached-wallpaper (&optional (imginfo nil))
  "Checks if the cached wallpaper is up to date, gets the latest one if not."
  (let* ((imginfo (or imginfo (get-bing-img-info :index 0)))
		 (startdate (assoc-value imginfo :startdate)) ; "YYYYMMDD"
		 (startts (encode-timestamp 0 0 0 0
								  (parse-integer startdate :start 6 :end 8)
								  (parse-integer startdate :start 4 :end 6)
								  (parse-integer startdate :end 4)))
		 (cachesecs (or (and (probe-file *wallpaper-filename*) (file-write-date *wallpaper-filename*)) 0))
		 (cachets (universal-to-timestamp cachesecs)))
	(when (timestamp< cachets startts)
	  (ensure-directories-exist *wallpaper-filename*)
	  (write-byte-vector-into-file (drakma:http-request (concatenate 'string "https://www.bing.com" (assoc-value imginfo :url))) *wallpaper-filename* :if-exists :supersede)
	  (with-open-file (s *wpinfo-filename* :direction :output :if-exists :supersede :if-does-not-exist :create)
		(write-line (assoc-value imginfo :title) s)
		(write-line (assoc-value imginfo :copyright) s)))
	(timestamp< cachets startts)))

(defun setup-wallpaper ()
  "Tries to set-up the wallpaper, if there's a network error then use the cached one."
  (let (title description imginfo)
	;; load the cached title and description first, if it exists
	(handler-case
	  (with-open-file (stream *wpinfo-filename*)
		(setf title (read-line stream))
		(setf description (read-line stream)))
	  (file-error (*) )
	  (stream-error (*)) )
	;; try to get updated info from cyberspace
	(handler-case
		(progn
		  (setf imginfo (get-bing-img-info))
		  (setf title (assoc-value imginfo :title))
		  (setf description (assoc-value imginfo :copyright))
		  (update-cached-wallpaper imginfo))
	  (usocket:socket-error (*) )
	  (usocket:ns-error (*) ))
	(when (and title description)
	  (setf *wallpaper-info* (format nil *wpinfo-fmt* title description))))
  (stumpwm:message *wallpaper-info*)
  ;; always try to load the wallpaper anyway
  (stumpwm:run-shell-command (format nil "feh --bg-fill ~A" *wallpaper-filename*)))

(defun bingwallpaper (&optional (delay 5))
  "Sets the wallpaper after DELAY seconds."
  (stumpwm:run-with-timer delay nil #'setup-wallpaper))

(stumpwm:defcommand load-bing-wallpaper (index) ((:number "From how many days ago? "))
  (set-bing-wallpaper :index index))

(stumpwm:defcommand bing-wallpaper-info () ()
  (stumpwm:message *wallpaper-info*))
