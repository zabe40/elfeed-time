;;; elfeed-time.el --- Display an entry's length in elfeed -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zabe

;; Author: zabe <zabe@disroot.org>
;; URL: https://github.com/zabe40/elfeed-time
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.2"))
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'elfeed)
(require 'dom)
(require 'cl-lib)

(defgroup elfeed-time nil
  "Display the length of an article, video, or podcast in elfeed."
  :group 'elfeed)

(defcustom elfeed-time-reading-speed 200
  "The user's reading speed, in words per minute. Must not be 0."
  :group 'elfeed-time
  :type 'number)

(defcustom elfeed-time-format-string "%h:%z%.2m:%.2s"
  "The format control string for displaying times for entries.
For information on possible specifiers, see `format-seconds'."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-use-curl elfeed-use-curl
  "If non-nil, fetch full content using curl instead of `url-retrieve'."
  :group 'elfeed-time
  :type 'boolean)

(defvar-local elfeed-time-preprocess-function nil
  "A function called to transform an entry's content before it is displayed.")

(defcustom elfeed-time-youtube-dl-program (or (executable-find "yt-dlp")
					      (executable-find "yt-dlc")
					      (executable-find "youtube-dl"))
  "The location of the program used to get video info."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-youtube-dl-args '(("yt-dlp" . ("--print" "%()j"))
					 ("yt-dlc" . ("--dump-json"))
					 ("youtube-dl" . ("--dump-json"))
					 (t . ("-q")))
  "An alist of associating program names to lists of arguments to pass to them.
The entry with key t is a list of arguments for all programs."
  :group 'elfeed-time
  :type '(alist :key-type (choice string
				  (const :tag "Arguments for all programs" t))
		:value-type (repeat string)))

(defcustom elfeed-time-ignore-private-videos nil
  "Whether or not to ignore private youtube videos.
This can be t, nil, or a function which is called with the entry
and returns t or nil. Ignoring means the entry will automatically
be marked as read."
  :group 'elfeed-time
  :type '(choice (const :tag "Don't ignore" nil)
		 (const :tag "Ignore" t)
		 function))

(defvar-local elfeed-time--premiere-entry nil
  "Used to store the elfeed-entry object for extra processes.")

(defvar-local elfeed-time--get-video-info-entry nil
  "Used to store the elfeed-entry object while getting it's information.")

(defcustom elfeed-time-ffprobe-program-name (executable-find "ffprobe")
  "The location of the program used to get enclosure info."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-ffprobe-arguments
  '("-hide_banner"
    "-loglevel error"
    "-print_format default=noprint_wrappers=1:nokey=1"
    "-show_entries format=duration")
  "A list of arguments to pass to ffprobe to get the duration of a media file in seconds."
  :group 'elfeed-time
  :type '(repeat string))

(defvar elfeed-time-ffprobe-format-cache ()
  "A list of file/url extensions supported by ffprobe.")

(defface elfeed-time-display '((t :inherit (elfeed-search-unread-count-face)))
  "Face for displaying the amount of time it takes to read,
  watch, or listen to an entry.")

(defvar elfeed-time-gc-functions nil
  "A list of functions called to determine which elfeed-refs are reachable.
Each function must take an elfeed-entry and return a single
elfeed-ref or a list of them, which will be considered in-use by
`elfeed-db-gc'.")

(defun elfeed-time-db-gc-trace-meta (&optional stats-p)
  "Clean up unused content from the content database.
If STATS is true, return the space cleared in bytes."
  (elfeed-db-gc-empty-feeds)
  (let* ((data (expand-file-name "data" elfeed-db-directory))
         (dirs (directory-files data t "^[0-9a-z]\\{2\\}$"))
         (ids (cl-mapcan (lambda (d) (directory-files d nil nil t)) dirs))
         (table (make-hash-table :test 'equal)))
    (dolist (id ids)
      (setf (gethash id table) nil))
    (with-elfeed-db-visit (entry _)
      (let ((content (elfeed-entry-content entry)))
        (when (elfeed-ref-p content)
          (setf (gethash (elfeed-ref-id content) table) t)))
      (dolist (trace-function elfeed-time-gc-functions)
        (let ((refs (funcall trace-function entry)))
	  (unless (listp refs)
            (setf refs (list refs)))
	  (dolist (ref refs)
            (when (elfeed-ref-p ref)
	      (setf (gethash (elfeed-ref-id ref) table) t))))))
    (cl-loop for id hash-keys of table using (hash-value used)
             for used-p = (or used (member id '("." "..")))
             when (and (not used-p) stats-p)
             sum (let* ((ref (elfeed-ref--create :id id))
                        (file (elfeed-ref--file ref)))
                   (* 1.0 (nth 7 (file-attributes file))))
             unless used-p
             do (elfeed-ref-delete (elfeed-ref--create :id id))
             finally (cl-loop for dir in dirs
			      when (elfeed-directory-empty-p dir)
			      do (delete-directory dir)))))

(defun elfeed-time-entry-meta-content (entry)
  "Return the elfeed-ref in ENTRY's content meta slot."
  (elfeed-meta entry :et-content))
(add-hook 'elfeed-time-gc-functions #'elfeed-time-entry-meta-content)

(defun elfeed-time-current-entries (multiple-entries-p)
  "Return the current selected entry or entries in either elfeed mode.
Return a list of entries when MULTIPLE-ENTRIES-P is non-nil,
otherwise, return the primary selected entry."
  (cl-case major-mode
    (elfeed-search-mode (elfeed-search-selected (not multiple-entries-p)))
    (elfeed-show-mode (if multiple-entries-p
			  (list elfeed-show-entry)
			elfeed-show-entry))
    (t (user-error "Can't get current entries. Not in an elfeed mode"))))

(defun elfeed-time-format-seconds (string seconds)
  "Use format control STRING to format the number SECONDS.
When SECONDS is negative, take its absolute value and prepend a
negative sign. All format specifiers are as in `format-seconds'."
  (concat (if (< seconds 0) "-" "")
	  (format-seconds string (abs seconds))))

(defun elfeed-time-format-seconds-max-length (format-string max-seconds)
  "Return the maximum length in characters that FORMAT-STRING can be.
Use MAX-SECONDS as the largest time to expect."
  (1+ (length (elfeed-time-format-seconds format-string (abs max-seconds)))))

(defun elfeed-time-curl-args (url &optional headers method data)
  "Build an argument list for curl for URL.
URL must be one string.

Adapted from `elfeed-curl--args'"
  (cl-assert (stringp url) t (format "URL must be a single string, instead of %S" url))
  (let ((args ())
        (capabilities (elfeed-curl-get-capabilities)))
    (push "--disable" args)
    (when (plist-get capabilities :compression)
      (push "--compressed" args))
    (push "--silent" args)
    (push "--location" args)
    (push (format "-m%s" 5) args)
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (push (format "-H%s: %s" key value) args)))
    (when method (push (format "-X%s" method) args))
    (when data (push (format "-d%s" data) args))
    (setf args (nconc (reverse elfeed-curl-extra-arguments) args))
    (nreverse (cons url args))))

;; TODO make this run asynchronously
;; TODO timeout after some time
(defun elfeed-time-fetch-full-content (entry)
  "Fetch the full content of ENTRY."
  (interactive (list (elfeed-time-current-entries nil)))
  (when (or (member 'preview (elfeed-entry-tags entry))
	    (called-interactively-p 'any))
    (with-current-buffer
	(if elfeed-time-use-curl
	    (let ((buffer (generate-new-buffer " *curl-full-content*")))
	      (call-process-shell-command
	       (concat elfeed-curl-program-name " "
		       (mapconcat #'identity (elfeed-time-curl-args (elfeed-entry-link entry)
								       `(("User-Agent" . ,elfeed-user-agent)))
				  " "))
	       nil buffer nil)
	      buffer)
	  (url-retrieve-synchronously (elfeed-entry-link entry) nil nil 30))
      (setf (elfeed-meta entry :et-content) (elfeed-ref (buffer-string))
	    (elfeed-meta entry :et-content-type) 'html)
      (elfeed-untag entry 'preview)
      (kill-buffer))))

(defun elfeed-time-serialize-dom (dom)
  "Serialize an HTML or XML DOM into a string."
  (let ((dom-depth (or (bound-and-true-p dom-depth) 0)))
    (cl-typecase dom
      (string dom)
      (t (let ((tag (dom-tag dom))
	       (attributes (dom-attributes dom))
	       (children (dom-children dom))
	       (dom-depth (1+ dom-depth)))
	   (format "<%s %s>\n%s</%s>"
		   tag
		   (mapconcat (lambda (attribute-pair)
				(format "%s=\"%s\""
					(car attribute-pair)
					(cdr attribute-pair)))
			      attributes " ")
		   (mapconcat (lambda (child)
				(concat (make-string dom-depth ?\s)
					(elfeed-time-serialize-dom child)))
			      children "\n")
		   tag))))))

(defun elfeed-time-make-html-readable (html base)
  "Return the main \"readable\" parts of HTML.
Use BASE as the default url for relative links.

Adapted from `eww-readable'"
  (require 'eww)
  (save-excursion
    (let ((dom (with-temp-buffer
		 (insert html)
		 (condition-case nil
		     (decode-coding-region (point-min) (point-max) 'utf-8)
		   (coding-system-error nil))
                 (eww--preprocess-html (point-min) (point-max))
		 (libxml-parse-html-region (point-min) (point-max)))))
      (eww-score-readability dom)
      (elfeed-time-serialize-dom (list 'base (list (cons 'href base))
				       (eww-highest-readability dom))))))

(defun elfeed-time-extract-readable-content (entry)
  "Set :et-content meta of ENTRY to a cleaned-up version of the same HTML (if applicable)"
  (interactive (list (elfeed-time-current-entries nil)))
  (when (or (member 'unreadable  (elfeed-entry-tags entry))
	    (called-interactively-p 'any))
    (let* ((meta-content-p (elfeed-meta entry :et-content))
	   (content (if meta-content-p
			(elfeed-meta entry :et-content)
		      (elfeed-entry-content entry)))
	   (content-type (if meta-content-p
			     (elfeed-meta entry :et-content-type)
			   (elfeed-entry-content-type entry))))
      (when (eq 'html content-type)
	(let* ((feed (elfeed-entry-feed entry))
	       (base (elfeed-compute-base (elfeed-feed-url feed))))
	  (with-temp-buffer
	    (insert (elfeed-deref content))
	    (setf (buffer-string) (elfeed-time-make-html-readable (buffer-string) (and feed base)))
	    (setf (elfeed-meta entry :et-content) (elfeed-ref (buffer-string)))
	    (elfeed-untag entry 'unreadable)))))))

(defun elfeed-time-preprocess-content-readable (content content-type base)
  (if (eq content-type 'html)
      (elfeed-time-make-html-readable content base)
    content))

(defun elfeed-time-generate-refresh-mail-style-function ()
  "Return a function to update the buffer to match the selected entry, using a mail-style.
When `elfeed-time-preprocess-function' is non-nil, call it on
the entry's content before displaying it.

Adapted from `elfeed-show-refresh--mail-style'"
  (lambda ()
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (authors (elfeed-meta elfeed-show-entry :authors))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
	   (time-marker (make-marker))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
	   (meta-content-p (elfeed-meta elfeed-show-entry :et-content))
	   (content (elfeed-deref (if meta-content-p
				      (elfeed-meta elfeed-show-entry :et-content)
				    (elfeed-entry-content elfeed-show-entry))))
           (type (if meta-content-p
		     (elfeed-meta elfeed-show-entry :et-content-type)
		   (elfeed-entry-content-type elfeed-show-entry)))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
		      (propertize title 'face 'message-header-subject)))
      (when elfeed-show-entry-author
	(dolist (author authors)
          (let ((formatted (elfeed--show-format-author author)))
            (insert
	     (format (propertize "Author: %s\n" 'face 'message-header-name)
		     (propertize formatted 'face 'message-header-to))))))
      (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
		      (propertize nicedate 'face 'message-header-other)))
      (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
		      (propertize feed-title 'face 'message-header-other)))
      (when tags
	(insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
			(propertize tagsstr 'face 'message-header-other))))
      (set-marker time-marker (point))
      (insert (propertize "Link: " 'face 'message-header-name))
      (elfeed-insert-link link link)
      (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
	       do (insert (propertize "Enclosure: " 'face 'message-header-name))
	       do (elfeed-insert-link (car enclosure))
	       do (insert "\n"))
      (insert "\n")
      (if content
	  (progn (when elfeed-time-preprocess-function
		   (setf content (funcall elfeed-time-preprocess-function content type base)))
		 (if (eq type 'html)
		     (elfeed-insert-html content base)
		   (insert content)))
	(insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))
      (when (or (< (window-end (selected-window) t)
		   (point-max))
		(integerp (elfeed-meta elfeed-show-entry :et-length-in-seconds)))
	(goto-char time-marker)
	(insert (format (propertize "Time: %s\n" 'face 'message-header-name)
			(propertize (elfeed-time-format-seconds elfeed-time-format-string
						       (elfeed-time-compute-entry-time elfeed-show-entry))
				    'face 'message-header-other)))
	(goto-char (point-min)))
      (set-marker time-marker nil))))


(defun elfeed-time-toggle-entry-readable ()
  "Show the cleaned-up version of the current entry's content
without storing it permanently"
  (interactive)
  (if (elfeed-meta elfeed-show-entry :et-content)
      (setq-local elfeed-time-preprocess-function #'elfeed-time-preprocess-content-readable) nil nil)
  (elfeed-show-refresh))

(defun elfeed-time-premiere-parse (entry buffer-string)
  "Parse the HTML source of the premiere associated with ENTRY
to determine when it will go live."
  (when (string-match (rx "\"startTimestamp\":\""
			  (group (1+ (not "\""))) "\"")
		      buffer-string)
    (setf (elfeed-meta entry :et-premiere-time)
	  (time-convert (encode-time (iso8601-parse
				      (match-string 1 buffer-string)))
			'integer))
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))))

(defun elfeed-time-premiere-sentinel (process event-string)
  "Kill the buffer associated with PROCESS when EVENT-STRING
indicates the process is finished."
  (with-current-buffer (process-buffer process)
    (when (string-match-p (rx "finished") event-string)
      (elfeed-time-premiere-parse elfeed-time--premiere-entry (buffer-string)))
    (unless (process-live-p process)
      (kill-buffer))))

(cl-defun elfeed-time-premiere-callback (&rest rest &aux (entry (plist-get (cdr rest) :entry)))
  "Kill the buffer associated with this callback after parsing it."
  (elfeed-time-premiere-parse entry (buffer-string))
  (kill-buffer (current-buffer)))

(defun elfeed-time-get-youtube-premiere-info (entry)
  "Fetch the time when ENTRY will go live as a Youtube Premiere."
  (interactive (list (elfeed-time-current-entries nil)))
  (when (string-match-p "youtube\\.com" (elfeed-entry-link entry))
    (if elfeed-time-use-curl
	(make-process :name "elfeed-time-get-premiere-time"
		      :buffer (with-current-buffer (generate-new-buffer " *elfeed-time-premiere-time*")
				(setf elfeed-time--premiere-entry entry)
				(current-buffer))
		      :command (cl-list* elfeed-curl-program-name
					 (elfeed-time-curl-args
					  (elfeed-entry-link entry)))
		      :connection-type 'pipe
		      :noquery nil
		      :sentinel #'elfeed-time-premiere-sentinel)
      (url-retrieve (elfeed-entry-link entry)
		    #'elfeed-time-premiere-callback
		    (list :entry entry)))))

(cl-defun elfeed-time-youtube-dl-args (&optional (program elfeed-time-youtube-dl-program))
  "Return a list of arguments to pass to `elfeed-time-youtube-dl-program'."
  (append (alist-get t elfeed-time-youtube-dl-args)
	  (alist-get (file-name-base program)
		     elfeed-time-youtube-dl-args
		     nil nil #'equal)))

(defun elfeed-time-get-video-info (entry)
  "Fetch additonal metadata about ENTRY such as length, description, etc."
  (interactive (list (elfeed-time-current-entries nil)))
  (when (string-match-p "youtube\\.com" (elfeed-entry-link entry))
    (message "Getting info for: %s" (elfeed-entry-link entry))
    (make-process :name "elfeed-time-get-video-length"
		  :buffer (with-current-buffer (generate-new-buffer " *elfeed-time-video-length*")
			    (setf elfeed-time--get-video-info-entry entry)
			    (current-buffer))
		  :command (cl-list* elfeed-time-youtube-dl-program
				     (elfeed-entry-link entry)
				     (elfeed-time-youtube-dl-args))
		  :connection-type 'pipe
		  :noquery nil
		  :sentinel
		  (lambda (process event-string)
		    (message "%s" event-string)
		    (catch 'unknown-error
		      (with-current-buffer (process-buffer process)
			(let ((entry elfeed-time--get-video-info-entry))
			  (pcase event-string
			    ((rx "exited abnormally")
			     (pcase (buffer-string)
			       ((rx "ERROR: This live event will begin in a few moments.")
				(setf (elfeed-meta entry :et-premiere-time) (time-convert nil 'integer)))
			       ((rx line-start "ERROR: " (* anychar)
				    (or "Premieres in" "This live event"))
				(elfeed-time-get-youtube-premiere-info entry))
			       ((rx "ERROR: Private video")
				(when (elfeed-meta entry :et-premiere-time)
				  (setf (elfeed-meta entry :et-premiere-time) nil))
				(when (or (and (functionp elfeed-time-ignore-private-videos)
					       (funcall elfeed-time-ignore-private-videos entry))
					  elfeed-time-ignore-private-videos)
				  (elfeed-untag entry 'unread))
				(message "%s is a private video" (elfeed-entry-link entry)))
			       ((rx "ERROR: " (* anychar)
				    "Sign in to confirm your age")
				(message "%s is agegated" (elfeed-entry-link entry)))
			       (_ (throw 'unknown-error nil))))
			    ((rx "finished")
			     (let ((video-data (progn (goto-char (point-min))
						      (json-parse-buffer))))
			       (setf (elfeed-meta entry :et-length-in-seconds) (gethash "duration" video-data)
				     (elfeed-meta entry :et-content) (elfeed-ref (gethash "description" video-data)))
			       (with-current-buffer (elfeed-search-buffer)
				 (elfeed-search-update-entry entry))
			       (when-let ((buffer-name (get-buffer (elfeed-show--buffer-name entry))))
				 (with-current-buffer buffer-name
				   (when (equal elfeed-show-entry entry)
				     (elfeed-show-refresh))))))
			    (_ (throw 'unknown-error nil)))
			  (kill-buffer (process-buffer process)))))))))

(defun elfeed-time-count-entry-words (entry)
  "Add the word count of ENTRY to the entry's metadata.

Adapted from `elfeed-show-refresh--mail-style'."
  (interactive (list (elfeed-time-current-entries nil)))
  (let* ((meta-content-p (elfeed-meta entry :et-content))
	 (type (if meta-content-p
		   (elfeed-meta entry :et-content-type)
		 (elfeed-entry-content-type entry)))
	 (feed (elfeed-entry-feed entry))
	 (content (elfeed-deref (if meta-content-p
				    (elfeed-meta entry :et-content)
				  (elfeed-entry-content entry))))
	 (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (with-temp-buffer
      (when content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content)))
      (setf (elfeed-meta entry :et-word-count)
	    (count-words (point-min) (point-max))))))

(defun elfeed-time-ffprobe-supported-extensions ()
  "Return a list of extensions supported by ffprobe."
  (unless elfeed-time-ffprobe-format-cache
    (setf elfeed-time-ffprobe-format-cache
	  (split-string (shell-command-to-string
			 (format "%s %s | %s | %s"
				 elfeed-time-ffprobe-program-name
				 "-hide_banner -demuxers"
				 ;; skip section header
				 "sed -e '1,/--/d'"
				 ;; filter to second column
				 "awk '{print $2}'"))
			(rx (any "\n,")) t)))
  elfeed-time-ffprobe-format-cache)

(defun elfeed-time-get-enclosure-time (entry)
  "Store the length of the longest enclosure of ENTRY in ENTRY's meta plist."
  (when-let ((enclosures (seq-filter (lambda (url)
				       (member (substring (url-file-extension url) 1)

					       (elfeed-time-ffprobe-supported-extensions)))
				     (mapcar #'car (elfeed-entry-enclosures entry)))))
    (setf (elfeed-meta entry :et-length-in-seconds)
	  (cl-loop for url in enclosures
		   maximizing (cl-parse-integer
			       (shell-command-to-string
				(mapconcat #'identity
					   (cl-list* elfeed-time-ffprobe-program-name
						     (concat "\"" url "\"")
						     elfeed-time-ffprobe-arguments)
					   " "))
			       :junk-allowed t)))))

;; TODO make it toggleable whether or not this is synchronous or asyncronous
(defun elfeed-time-new-entry (entry)
  "Store the time it will take to read/watch/listen to ENTRY.
This is called once per entry, as a part of
`elfeed-new-entry-hook', and should be used for longer
operations, such as network requests."
  (cond
   ((member 'video (elfeed-entry-tags entry)) (elfeed-time-get-video-info entry))
   ((member 'comics (elfeed-entry-tags entry)) nil) ;TODO find a way to estimate reading time for comics
   ((member 'podcast (elfeed-entry-tags entry)) (elfeed-time-get-enclosure-time entry))
   (t (elfeed-time-count-entry-words entry))))

(defun elfeed-time-compute-entry-time (entry)
  "Return the time it will take to read/watch/listen to ENTRY.
This is called many times as a part of sorting the elfeed-search
buffer by entry-time, and should be used only for fast
operations."
  (or (let ((length (elfeed-meta entry :et-length-in-seconds)))
	(when (numberp length)
	  length))
      (when (elfeed-meta entry :et-premiere-time)
	(- (time-convert nil 'integer) (elfeed-meta entry :et-premiere-time)))
      (when (and (numberp (elfeed-meta entry :et-word-count))
		 (not (zerop elfeed-time-reading-speed)))
	(/ (* 60
	      (elfeed-meta entry :et-word-count))
	   elfeed-time-reading-speed))
      0))

;; TODO account for cjk chars
;; see "min-width" display specification in emacs 28
;; https://lars.ingebrigtsen.no/2021/11/24/the-most-controversial-change-in-emacs-history/
(defun elfeed-time-search-print-entry (entry)
  "Print ENTRY to the `elfeed-search-mode' buffer.

 Adapted from `elfeed-search-print-entry--default'"
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
			       elfeed-search-title-min-width
			       title-width
			       elfeed-search-title-max-width)
                        :left))
	 (time (elfeed-format-column (elfeed-time-format-seconds (concat elfeed-time-format-string " ")
								 (elfeed-time-compute-entry-time entry))
				     (1+ (elfeed-time-format-seconds-max-length elfeed-time-format-string
										(* 1 60 60 24 30)))
				     :right)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    ;; The faces are reversed so that faces earlier in
    ;; `elfeed-search-face-alist' are applied later, and thus
    ;; override faces already added
    (dolist (face (reverse title-faces))
      (add-face-text-property 0 (length title-column) face nil title-column))
    (insert (propertize title-column 'kbd-help title) " ")
    (insert (propertize time 'face 'elfeed-time-display))
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-time-compare-entries (entry-1 entry-2)
  "Compare how long it would take to read ENTRY-1 and ENTRY-2.

This function can be used as the `elfeed-search-sort-function',
and is therefore also suitable as the predicate for `sort'."
  (< (elfeed-time-compute-entry-time entry-1)
     (elfeed-time-compute-entry-time entry-2)))


(defun elfeed-time-set-sort-function (function)
  "Sort the elfeed-search buffer according to FUNCTION immediately."
  (setf elfeed-search-sort-function function)
  (elfeed-search-update--force))

(defun elfeed-time-sort-by-date ()
  "Sort the elfeed-search buffer by date."
  (interactive)
  (elfeed-time-set-sort-function nil))

(defun elfeed-time-sort-by-time ()
  "Sort the elfeed-search buffer by time to read entry."
  (interactive)
  (elfeed-time-set-sort-function #'elfeed-time-compare-entries))

(defun elfeed-time-toggle-sort-order ()
  "Reverse the order of the elfeed-search buffer."
  (interactive)
  (setf elfeed-sort-order (cl-case elfeed-sort-order
			    (ascending 'descending)
			    (descending 'ascending)))
  (elfeed-search-update--force))

(provide 'elfeed-time)
;;; elfeed-time.el ends here

