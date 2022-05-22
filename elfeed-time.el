;;; elfeed-time.el --- Display an entry's length in elfeed -*- lexical-binding: t; -*-

;; Copyright (C) 2021,2022 zabe

;; Author: zabe <zabe@disroot.org>
;; URL: https://github.com/zabe40/elfeed-time
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.1"))
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

;; elfeed-time displays the approximate time it will take to read,
;; watch, or listen to an elfeed entry. It can display this
;; information in both elfeed-search-mode, and elfeed-show-mode.
;;
;; elfeed-time can measure the length of:
;; * Youtube videos
;; * Youtube premieres
;; * podcasts
;; * and of course text (including feeds that only give a preview)
;;
;; For more information about elfeed-time, see README.org

;;; Code:

(require 'elfeed)
(require 'dom)
(require 'shr)
(require 'cl-lib)
(require 'ietf-drums)
(require 'iso8601)

(defgroup elfeed-time nil
  "Display the length of an article, video, or podcast in elfeed."
  :group 'elfeed)

(defcustom elfeed-time-reading-speed 200
  "The user's reading speed, in words per minute. Must not be 0."
  :group 'elfeed-time
  :type 'number)

(defcustom elfeed-time-speed-multiplier nil
  "The default speed multiplier for videos and podcasts. Must not be 0.
A value of nil means normal speed, that is, 1."
  :group 'elfeed-time
  :type '(choice number
		 (const :tag "Normal speed" nil))
  :set (lambda (symbol value)
	 (if (and (numberp value) (zerop value))
	     (user-error "%S must not be zero" symbol)
	   (set symbol value))))

(defcustom elfeed-time-format-string "%h:%z%.2m:%.2s"
  "The format control string for displaying times for entries.
For information on possible specifiers, see `format-seconds'."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-premiere-date-format-string "%Y-%m-%d at %X"
  "The format control string to display the date and time of premieres.
For information on possible specifiers, see
`format-time-string'."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-update-entries-after-hook-p nil
  "Whether or not to automatically update buffers after async functions."
  :group 'elfeed-time
  :type 'boolean)

(defcustom elfeed-time-use-curl elfeed-use-curl
  "If non-nil, fetch full content using curl instead of `url-retrieve'."
  :group 'elfeed-time
  :type 'boolean)

(defcustom elfeed-time-preview-tag 'preview
  "A tag for entries with only a preview of the desired content."
  :group 'elfeed-time
  :type 'symbol)

(defcustom elfeed-time-unreadable-tag 'unreadable
  "A tag for entries whose content contains extraneous elements.
This could include headers, footers, advertisements, etc."
  :group 'elfeed-time
  :type 'symbol)

(defcustom elfeed-time-video-tag 'video
  "A tag for entries that are videos."
  :group 'elfeed-time
  :type 'symbol)

(defcustom elfeed-time-podcast-tag 'podcast
  "A tag for entries that are podcasts."
  :group 'elfeed-time
  :type 'symbol)

(defcustom elfeed-time-premiere-tag 'premiere
  "A tag for entries that are premieres."
  :group 'elfeed-time
  :type 'symbol)

(defcustom elfeed-time-youtube-dl-program (or (executable-find "yt-dlp")
					      (executable-find "yt-dlc")
					      (executable-find "youtube-dl"))
  "The location of the program used to get video info."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-youtube-dl-args '(("yt-dlp" . ("--print" "%()j" "--no-colors"))
					 ("yt-dlc" . ("--dump-json" "--no-color"))
					 ("youtube-dl" . ("--dump-json" "--no-color"))
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

(defcustom elfeed-time-ffprobe-program-name (executable-find "ffprobe")
  "The location of the program used to get enclosure info."
  :group 'elfeed-time
  :type 'string)

(defcustom elfeed-time-ffprobe-arguments
  (list "-hide_banner"
	"-loglevel" "error"
	"-print_format" "default=noprint_wrappers=1:nokey=1"
	"-show_entries" "format=duration")
  "A list of arguments for ffprobe to get the length of a file."
  :group 'elfeed-time
  :type '(repeat string))

(defcustom elfeed-time-enclosure-reduce-function #'max
  "The function to determine the final entry length.
This function is called like so:

\(`cl-reduce' elfeed-time-enclosure-reduce-function LIST\)

where LIST is a list of the lengths of all supported enclosures.
LIST is guaranteed to be non-nil."
  :group 'elfeed-time
  :type 'function
  :options '(max +))

(defcustom elfeed-time-new-entry-functions nil
  "A list of functions to run asynchronously on new entries.
The functions in this hook are called with two arguments, an
ENTRY, and a CONTINUATION.

CONTINUATION is a list of functions. Functions in this hook
should arrange to call the first function in the list with ENTRY,
and the rest of CONTINUATION as the two arguments. Functions
should not call CONTINUATION if they encounter an error.
Functions in this hook will not be called with a value of nil for
CONTINUATION (but may want to handle that case anyways).

This hook is intended as a place to put computations that might
take a long time, including network requests, etc."
  :group 'elfeed-time
  :type 'hook
  :options '(elfeed-time-maybe-get-video-info
	     elfeed-time-maybe-get-premiere-info
	     elfeed-time-maybe-get-podcast-info
	     elfeed-time-maybe-get-full-content
	     elfeed-time-maybe-make-entry-readable
	     elfeed-time-count-entry-words))

(defcustom elfeed-time-entry-time-functions nil
  "A list of functions to call to compute an entry's time.
Functions are called in order with one argument, ENTRY, the
elfeed-entry they should compute the time of. Each function
should return an integer representing the time in seconds it
takes to read ENTRY, or nil if ENTRY is not of an applicable
type. A function can return a negative number of seconds to
indicate that ENTRY respresents an event in the future.

This hook is intended to be called within
`elfeed-search-sort-function' (which runs every keystroke in live
search), so functions in this hook should complete quickly,
especially if returning nil."
  :group 'elfeed-time
  :type 'hook
  :options '(elfeed-time-video-time
	     elfeed-time-premiere-time
	     elfeed-time-podcast-time
	     elfeed-time-text-time))

(defface elfeed-time-display '((t :inherit (elfeed-search-unread-count-face)))
  "Face for displaying the amount of time it takes to read,
  watch, or listen to an entry.")

(defface elfeed-time-sum '((t :inherit (elfeed-search-unread-count-face)))
  "Face for displaying the sum of times of entries.")

(defvar-local elfeed-time-preprocess-function nil
  "A function called to transform an entry's content before it is displayed.")

(defvar elfeed-time-ffprobe-format-cache ()
  "A list of file/url extensions supported by ffprobe.")

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

(defun elfeed-time-update-entry (entry)
  "Update all elfeed buffers displaying ENTRY."
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-search-update-entry entry))
  (when-let ((buffer-name (get-buffer (elfeed-show--buffer-name entry))))
    (with-current-buffer buffer-name
      (when (equal elfeed-show-entry entry)
	(elfeed-show-refresh)))))

(defun elfeed-time-update-feed (feed)
  "Update all elfeed buffers displaying an entry from FEED."
  (with-current-buffer (elfeed-search-buffer)
    (dolist (entry elfeed-search-entries)
      (when (equal feed (elfeed-entry-feed entry))
	(elfeed-search-update-entry entry))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'elfeed-show-mode)
		 (equal feed (elfeed-entry-feed elfeed-show-entry)))
	(elfeed-show-refresh)))))

(defun elfeed-time-log (level entry format &rest objects)
  "Write log message FORMAT at LEVEL about ENTRY to elfeed's log buffer.
FORMAT must be a string suitable for `format' given OBJECTS as arguments."
  (apply 'elfeed-log level
	 (concat "elfeed-time [%s]: " format)
	 (cons (elfeed-entry-link entry)
	       objects)))

(defun elfeed-time--process-sentinel (fun entry continuation)
  "Return a sentinel function suitable for `make-process'.
When the process exits, if no errors occur, call FUN with ENTRY,
the process buffer, and CONTINUATION as arguments. If errors
occur, log them."
  (lambda (process event-string)
    (when (not (process-live-p process))
      (if (string-match-p (rx "finished") event-string)
	  (funcall fun entry (process-buffer process) continuation)
	(elfeed-time-log 'error entry "%S (Exit code %s) %s"
			 event-string (process-exit-status process)
			 (with-current-buffer (process-buffer process)
			   (buffer-string)))))))

(defun elfeed-time--url-retrieve-callback (fun entry continuation)
  "Return a callback function suitable for `url-retrieve'.
When the callback is called, log all status events, and if no
errors occured, call FUN with ENTRY, the current buffer, and
CONTINUATION as arguments."
  (lambda (status &rest _rest)
    (let ((error-p nil))
      (while status
	(pcase (list (pop status) (pop status))
	  (`(:redirect ,redirect-to)
	   (elfeed-time-log 'debug entry
			    "Redirected to %S" redirect-to))
	  (`(:error (,error ,type . ,data))
	   (setf error-p t)
	   (elfeed-time-log 'error entry "%s, %s, %s"
			    error type data))))
      (unless error-p
	(funcall fun entry (current-buffer) continuation)))))

(cl-defun elfeed-time-youtube-dl-args (&optional (program elfeed-time-youtube-dl-program))
  "Return a list of arguments to pass to `elfeed-time-youtube-dl-program'."
  (append (alist-get t elfeed-time-youtube-dl-args)
	  (alist-get (file-name-base program)
		     elfeed-time-youtube-dl-args
		     nil nil #'equal)))

(defun elfeed-time-maybe-get-video-info (entry continuation)
  "Get ENTRY's length as a video if it is one.
Call CONTINUATION when finished."
  (if (elfeed-tagged-p elfeed-time-video-tag entry)
      (elfeed-time-get-video-info entry continuation)
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-get-video-info (entry &optional continuation)
  "Fetch additonal metadata about ENTRY such as length, description, etc.
Call CONTINUATION when finished."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
  (make-process :name "elfeed-time-get-video-length"
		:buffer (generate-new-buffer
			 (format " *elfeed-time-video-length: %s*"
				 (elfeed-entry-link entry)))
		:command (cl-list* elfeed-time-youtube-dl-program
				   (elfeed-entry-link entry)
				   (elfeed-time-youtube-dl-args))
		:connection-type 'pipe
		:noquery t
		:sentinel
		(lambda (process event-string)
		  (catch 'unknown-error
		    (with-current-buffer (process-buffer process)
		      (pcase event-string
			((rx "exited abnormally")
			 (goto-char (point-min))
			 (cond ((re-search-forward (rx bol "ERROR: This live event will begin in a few moments.")
						   nil t)
				(setf (elfeed-meta entry :et-premiere-time)
				      (time-convert nil 'integer)))
			       ((re-search-forward (rx bol "ERROR: " (* anychar)
						       (or "Premieres in" "This live event"))
						   nil t)
				(elfeed-tag entry elfeed-time-premiere-tag))
			       ((re-search-forward (rx bol "ERROR: Private video") nil t)
				(when (elfeed-meta entry :et-premiere-time)
				  (setf (elfeed-meta entry :et-premiere-time) nil))
				(when (or (and (functionp elfeed-time-ignore-private-videos)
					       (funcall elfeed-time-ignore-private-videos entry))
					  elfeed-time-ignore-private-videos)
				  (elfeed-untag entry 'unread))
				(elfeed-time-log 'warn entry "is a private video"))
			       ((re-search-forward (rx bol "ERROR: " (* anychar)
						       "Sign in to confirm your age")
						   nil t)
				(elfeed-time-log 'warn entry "is agegated"))
			       (t
				(elfeed-time-log 'error entry "Unknown error: %S"
						 (buffer-string))
				(throw 'unknown-error nil))))
			((rx "finished")
			 (goto-char (point-min))
			 (while (re-search-forward (rx bol "WARNING: " (group (+ not-newline)) eol) nil t)
			   (elfeed-time-log 'warn entry "%s" (match-string 1)))
			 (let ((video-data (json-parse-buffer)))
			   (setf (elfeed-meta entry :et-length-in-seconds) (gethash "duration" video-data)
				 (elfeed-meta entry :et-content) (elfeed-ref (gethash "description" video-data)))))
			(_ (throw 'unknown-error nil)))
		      (kill-buffer (process-buffer process))
		      (funcall (car continuation) entry (cdr continuation)))))))

(defun elfeed-time-premiere-parse (entry buffer continuation)
  "Parse the HTML in BUFFER to determine when ENTRY goes live.
Call CONTINUATION when finished."
  (with-current-buffer buffer
    (elfeed-time--delete-http-header)
    (goto-char (point-min))
    (when (re-search-forward (rx "\"startTimestamp\":\""
				 (group (1+ (not "\""))) "\"")
			     nil t)
      (setf (elfeed-meta entry :et-premiere-time)
	    (time-convert (encode-time (iso8601-parse
					(match-string 1)))
			  'integer)))
    (goto-char (point-min))
    (when (re-search-forward (rx "<meta " (or "name" "itemprop") "=\"description\" "
				 "content=\"" (group (1+ (not "\""))) "\""))
      (setf (elfeed-meta entry :et-content)
	    (match-string 1)))
    (kill-buffer buffer))
  (funcall (car continuation) entry (cdr continuation)))

(defun elfeed-time-maybe-get-premiere-info (entry continuation)
  "Get info for ENTRY only if it is a premiere.
Call CONTINUATION when finished."
  (if (elfeed-tagged-p elfeed-time-premiere-tag entry)
      (elfeed-time-get-premiere-info entry continuation)
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-get-premiere-info (entry &optional continuation)
  "Fetch the time when ENTRY will go live as a Youtube Premiere.
Call CONTINUATION when finished."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
  (when (string-match-p (rx "youtube.com") (elfeed-entry-link entry))
    (if elfeed-time-use-curl
	(make-process :name "elfeed-time-get-premiere-time"
		      :buffer (generate-new-buffer
			       (format " *elfeed-time-premiere-time: %s*"
				       (elfeed-entry-link entry)))
		      :command (cl-list* elfeed-curl-program-name
					 (elfeed-time-curl-args
					  (elfeed-entry-link entry)))
		      :connection-type 'pipe
		      :noquery t
		      :sentinel (elfeed-time--process-sentinel
				 #'elfeed-time-premiere-parse
				 entry continuation))
      (url-retrieve (elfeed-entry-link entry)
		    (elfeed-time--url-retrieve-callback
		     #'elfeed-time-premiere-parse
		     entry continuation)
		    nil t))))

(defun elfeed-time-ffprobe-supported-extensions (&optional force-reload)
  "Return a list of extensions supported by ffprobe.
When FORCE-RELOAD is non-nil regenerate the list."
  (when (or force-reload (null elfeed-time-ffprobe-format-cache))
    (setf elfeed-time-ffprobe-format-cache nil)
    (let ((buffer (generate-new-buffer " *elfeed-time-ffprobe-demuxers*")))
      (with-current-buffer buffer
	(call-process elfeed-time-ffprobe-program-name nil
		      buffer
		      nil
		      "-hide_banner" "-demuxers")
	(goto-char (point-min))
	;; Skip past header
	(re-search-forward (rx "--" (* not-newline) "\n") nil t)
	(while (re-search-forward (rx bol (* " ") "D" (+ space)
				      (group (+ (not space))) (+ space)
				      (* not-newline) "\n")
				  nil t)
	  (dolist (ext (split-string (match-string 1) (rx ",") t))
	    (push ext elfeed-time-ffprobe-format-cache)))
	(kill-buffer))))
  elfeed-time-ffprobe-format-cache)

(defun elfeed-time-ffprobe-supports-enclosure-p (enclosure)
  "Return ENCLOSURE if it can be demuxed by ffprobe.
Otherwise, return nil."
  (when (member (substring (url-file-extension (car enclosure)) 1)
		(elfeed-time-ffprobe-supported-extensions))
    enclosure))

(defun elfeed-time--set-enclosure-time (enclosure)
  "Set the length of ENCLOSURE to the numeric value of BUFFER."
  (lambda (entry buffer continuation)
    (with-current-buffer buffer
      (when-let ((position (cl-position enclosure (elfeed-entry-enclosures entry)
					:test #'equal)))
	(setf (nth position (elfeed-meta entry :et-enclosure-times))
	      (string-to-number (buffer-string))))
      (kill-buffer))
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-maybe-get-podcast-info (entry continuation)
  "Get the length of ENTRY if its a podcast.
Call CONTINUATION when finished."
  (if (elfeed-tagged-p elfeed-time-podcast-tag entry)
      (elfeed-time-get-enclosure-time entry continuation)
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-get-enclosure-time (entry &optional continuation)
  "Store the length of the longest enclosure of ENTRY in ENTRY's meta plist.
Call CONTINUATION when finished."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
  (let ((enclosures (mapcar #'elfeed-time-ffprobe-supports-enclosure-p
			    (elfeed-entry-enclosures entry))))
    (when (cl-some #'identity enclosures)
      (setf (elfeed-meta entry :et-enclosure-times)
	    (make-list (length enclosures) nil))
      (dolist (enclosure enclosures)
	(when enclosure
	  (push (lambda (lambda-entry lambda-continuation)
		  (make-process :name "elfeed-time-get-enclosure-time"
				:buffer (generate-new-buffer
					 (format " *elfeed-time-get-enclosure-time*: %s"
						 (elfeed-entry-link lambda-entry)))
				:command (cl-list* elfeed-time-ffprobe-program-name
						   (car enclosure)
						   elfeed-time-ffprobe-arguments)
				:noquery t
				:connection-type 'pipe
				:sentinel (elfeed-time--process-sentinel
					   (elfeed-time--set-enclosure-time enclosure)
					   lambda-entry lambda-continuation)))
		continuation))))
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-curl-args (url &optional headers method data)
  "Build an argument list for curl for URL.
URL must be one string.

Adapted from `elfeed-curl--args'"
  (cl-assert (stringp url) t (format "URL must be a single string, instead of %S"
				     url))
  (let ((args ())
        (capabilities (elfeed-curl-get-capabilities)))
    (push "--disable" args)
    (when (plist-get capabilities :compression)
      (push "--compressed" args))
    (push "--silent" args)
    (push "--location" args)
    (push (format "-m%s" elfeed-curl-timeout) args)
    (push "-D-" args)
    (dolist (header headers)
      (cl-destructuring-bind (key . value) header
        (push (format "-H%s: %s" key value) args)))
    (when method (push (format "-X%s" method) args))
    (when data (push (format "-d%s" data) args))
    (setf args (nconc (reverse elfeed-curl-extra-arguments) args))
    (nreverse (cons url args))))

(defun elfeed-time--delete-http-header ()
  "Delete the HTTP header from the current buffer."
  (delete-region (point-min)
		 (save-restriction
		   (ietf-drums-narrow-to-header)
		   (point-max))))

(defun elfeed-time--set-full-content (entry buffer continuation)
  "Set ENTRY's content to BUFFER, then call CONTINUATION."
  (with-current-buffer buffer
    (elfeed-time--delete-http-header)
    (setf (elfeed-meta entry :et-content) (elfeed-ref (buffer-string))
	  (elfeed-meta entry :et-content-type) 'html)
    (elfeed-untag entry elfeed-time-preview-tag)
    (kill-buffer))
  (funcall (car continuation) entry (cdr continuation)))

(defun elfeed-time-maybe-get-full-content (entry continuation)
  "Get full content for ENTRY if it gives only a preview.
Call CONTINUATION when finished."
  (if (elfeed-tagged-p elfeed-time-preview-tag entry)
      (elfeed-time-get-full-content entry continuation)
    (funcall (car continuation) entry (cdr continuation))))

;; TODO timeout after some time
(defun elfeed-time-get-full-content (entry &optional continuation)
  "Get the full content of ENTRY, calling CONTINUATION when finished."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
  (if elfeed-time-use-curl
      (make-process :name "elfeed-time-full-content"
		    :buffer (generate-new-buffer
			     (format " *elfeed-time-full-content: %s*"
				     (elfeed-entry-link entry)))
		    :command (cl-list* elfeed-curl-program-name
				       (elfeed-time-curl-args
					(elfeed-entry-link entry)
					`(("User-Agent" . ,elfeed-user-agent))))
		    :connection-type 'pipe
		    :noquery t
		    :sentinel (elfeed-time--process-sentinel
			       #'elfeed-time--set-full-content
			       entry continuation))
    (url-retrieve (elfeed-entry-link entry)
		  (elfeed-time--url-retrieve-callback
		   #'elfeed-time--set-full-content
		   entry continuation)
		  nil t)))

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
      (with-temp-buffer
	(shr-dom-print (list 'base (list (cons 'href base))
			     (eww-highest-readability dom)))
	(buffer-string)))))

(defun elfeed-time-maybe-make-entry-readable (entry continuation)
  "Make ENTRY readable if it is unreadable.
Call CONTINUATION when finished."
  (if (elfeed-tagged-p elfeed-time-unreadable-tag entry)
      (elfeed-time-make-entry-readable entry continuation)
    (funcall (car continuation) entry (cdr continuation))))

(defun elfeed-time-make-entry-readable (entry &optional continuation)
  "Set :et-content of ENTRY to a cleaned-up version of the original HTML.
Call CONTINUATION when finished."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
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
	  (setf (buffer-string)
		(elfeed-time-make-html-readable (buffer-string)
						(and feed base)))
	  (setf (elfeed-meta entry :et-content)
		(elfeed-ref (buffer-string)))
	  (elfeed-untag entry elfeed-time-unreadable-tag)))))
  (funcall (car continuation) entry (cdr continuation)))

(defun elfeed-time-count-entry-words (entry &optional continuation)
  "Add the word count of ENTRY to the entry's metadata.
Call CONTINUATION when finished.

Adapted from `elfeed-show-refresh--mail-style'."
  (interactive (list (elfeed-time-current-entries nil)))
  (setf continuation (or continuation (list #'ignore)))
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
	    (count-words (point-min) (point-max)))))
  (funcall (car continuation) entry (cdr continuation)))

(defun elfeed-time-new-entry (entry)
  "Store the time it will take to read/watch/listen to ENTRY.
This is called once per entry, as a part of
`elfeed-new-entry-hook', and should be used for longer
operations, such as network requests."
  (let ((funs (append elfeed-time-new-entry-functions
		      (list (if elfeed-time-update-entries-after-hook-p
				(lambda (entry _continuation)
				  (elfeed-time-update-entry entry))
			      #'ignore)))))
    (funcall (car funs) entry (cdr funs))))

(defun elfeed-time-premiere-time (entry)
  "Reutrn the number of seconds after ENTRY's start, or nil if not a premiere.
The number returned will be negative if the premiere hasn't
started yet."
  (let ((start (elfeed-meta entry :et-premiere-time)))
    (when (numberp start)
      (- (time-convert nil 'integer) start))))

(defun elfeed-time-read-divisor (prompt &optional default)
  "Read a nonzero number from the minibuffer, prompting with PROMPT.
DEFAULT is as in `read-number'."
  (let ((answer 0))
    (while (= answer 0)
      (setf answer (read-number prompt default))
      (when (= answer 0)
	(message "Please enter a nonzero number")
	(sit-for 1.5)))
    answer))

(defun elfeed-time-set-feed-speed (feed speed)
  "Set FEED's :et-speed-multiplier to SPEED."
  (interactive (let ((feed (elfeed-entry-feed (elfeed-time-current-entries nil))))
		 (list feed
		       (elfeed-time-read-divisor
			"Speed: "
			(elfeed-meta feed :et-speed-multiplier)))))
  (setf (elfeed-meta feed :et-speed-multiplier) speed)
  (elfeed-time-update-feed feed))

(defun elfeed-time-reset-feed-speed (feed)
  "Reset FEED's speed-multiplier to 1."
  (interactive (let ((completion-extra-properties
		      ;; TODO use :affixation function in emacs 28
		      (list :annotation-function
			    (lambda (feed-url)
			      (when-let ((feed (gethash feed-url elfeed-db-feeds))
					 (speed (elfeed-meta
						 feed :et-speed-multiplier)))
				(format " %s %s"
					(or (elfeed-meta feed :title)
					    (elfeed-feed-title feed))
					speed))))))
		 (list (gethash (completing-read
				 "Feed: " elfeed-db-feeds
				 (lambda (key value)
				   (let ((speed (elfeed-meta
						 value :et-speed-multiplier)))
				     (and speed (not (equal 1 speed)))))
				 t)
				elfeed-db-feeds))))
  (setf (elfeed-meta feed :et-speed-multiplier) nil)
  (elfeed-time-update-feed feed))

(defun elfeed-time-scale-entry-time (entry seconds)
  "Return SECONDS scaled by the appropriate amount.
First check if ENTRY's feed specifies a multiplier, then check
`elfeed-time-speed-multiplier'. If neither are non-nil and
non-zero, then return SECONDS unchanged."
  (/ seconds
     (or (when-let ((speed (elfeed-meta (elfeed-entry-feed entry)
					:et-speed-multiplier))
		    ((not (zerop speed))))
	   speed)
	 (when-let ((speed elfeed-time-speed-multiplier)
		    ((not (zerop speed))))
	   speed)
	 1)))

(defun elfeed-time-video-time (entry)
  "Return the length of ENTRY as a video in seconds."
  (let ((length (elfeed-meta entry :et-length-in-seconds)))
    (when (numberp length)
      (elfeed-time-scale-entry-time entry length))))

(defun elfeed-time-podcast-time (entry)
  "Return the length of ENTRY as a podcast in seconds."
  (when-let ((all-enclosures (elfeed-meta entry :et-enclosure-times))
	     (enclosures (cl-remove-if-not #'numberp all-enclosures))
	     (length (cl-reduce elfeed-time-enclosure-reduce-function enclosures)))
    (elfeed-time-scale-entry-time entry length)))

(defun elfeed-time-text-time (entry)
  "Return the length of ENTRY as derived from it's word count."
  (when (and (numberp (elfeed-meta entry :et-word-count))
	     (not (zerop elfeed-time-reading-speed)))
    (/ (* 60 (elfeed-meta entry :et-word-count))
       elfeed-time-reading-speed)))

(defun elfeed-time-compute-entry-time (entry)
  "Return the time it will take to read/watch/listen to ENTRY.
This is called many times as a part of sorting the elfeed-search
buffer by entry-time, and should be used only for fast
operations."
  (cl-loop for fun in elfeed-time-entry-time-functions
	   when (funcall fun entry)
	   return it
	   finally return 0))

(defun elfeed-time-compare-entries (entry-1 entry-2)
  "Compare how long it would take to read ENTRY-1 and ENTRY-2.

This function can be used as the `elfeed-search-sort-function',
and is therefore also suitable as the predicate for `sort'."
  (< (elfeed-time-compute-entry-time entry-1)
     (elfeed-time-compute-entry-time entry-2)))

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

;; TODO account for cjk chars
;; see "min-width" display specification in emacs 29
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
         (title-column (bidi-string-mark-left-to-right
			(elfeed-format-column
                         title (elfeed-clamp
				elfeed-search-title-min-width
				title-width
				elfeed-search-title-max-width)
                         :left)))
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

(defun elfeed-time-preprocess-content-readable (content content-type base)
  (if (eq content-type 'html)
      (elfeed-time-make-html-readable content base)
    content))

(defun elfeed-time-refresh-mail-style-function ()
  "Update the buffer to match the selected entry, using a mail-style.
When `elfeed-time-preprocess-function' is non-nil, call it on the
entry's content before displaying it.

Adapted from `elfeed-show-refresh--mail-style'"
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (authors (elfeed-meta elfeed-show-entry :authors))
         (link (elfeed-entry-link elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
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
    (insert (format (propertize "Time: %s\n" 'face 'message-header-name)
		    (propertize (elfeed-time-format-seconds
				 elfeed-time-format-string
				 (elfeed-time-compute-entry-time
				  elfeed-show-entry))
				'face 'message-header-other)))
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
    (goto-char (point-min))))

;;; Convenience functions

(defun elfeed-time-print-premiere-date (entry)
  "Print the date of the start of ENTRY to the minibuffer.
ENTRY must represent a premiere."
  (interactive (list (elfeed-time-current-entries nil)))
  (let ((title (or (elfeed-meta entry :title)
		   (elfeed-entry-title entry)
		   "")))
    (if-let ((time (elfeed-meta entry :et-premiere-time)))
	(message "%s premieres on %s"
		 title
		 (format-time-string elfeed-time-premiere-date-format-string
				     time))
      (user-error "%s is not a premiere" title))))

(defun elfeed-time-sum-entry-times (entries &optional include-premieres-p)
  "Return the total time it would take to read ENTRIES.
When INCLUDE-PREMIERES-P is nil don't add premiere times to the
sum."
  (cl-loop for entry in entries
	   when (or include-premieres-p
		    (not (elfeed-tagged-p elfeed-time-premiere-tag entry)))
	   sum (elfeed-time-compute-entry-time entry)))

(defun elfeed-time-print-time-sum (entries &optional include-premieres-p)
  "Print the total time it would take to read ENTRIES.
ENTRIES defaults to all displayed entries, unless the region is
active, in which case ENTRIES is the list of selected entries.
With any non-nil prefix argument include premieres in this sum."
  (interactive (list (if (use-region-p)
			 (elfeed-time-current-entries t)
		       elfeed-search-entries)
		     current-prefix-arg))
  (message "%s" (elfeed-time-format-seconds
		 elfeed-time-format-string
		 (elfeed-time-sum-entry-times entries
					      include-premieres-p))))

(defun elfeed-time-header ()
  "Return a string to be used as the elfeed-search header.
This function is intended for use in
`elfeed-search-header-function'.

Adapted from `elfeed-search--header'."
  (format "%s%s"
	  (propertize (cond ((or (null elfeed-search-entries)
				 (zerop (elfeed-db-last-update))
				 (> (elfeed-queue-count-total) 0))
			     "")
			    ((and elfeed-search-filter-active
				  elfeed-search-filter-overflowing)
			     "??:?? ")
			    (t
			     (concat (elfeed-time-format-seconds
				      elfeed-time-format-string
				      (elfeed-time-sum-entry-times
				       (if (use-region-p)
					   (elfeed-search-selected nil)
					 (cl-loop for entry in elfeed-search-entries
						  when (catch 'elfeed-db-done
							 (elfeed-search-filter
							  (elfeed-search-parse-filter elfeed-search-filter) entry (elfeed-entry-feed entry)))
						  collect entry))))
				     " ")))
		      'face 'elfeed-time-sum)
	  (elfeed-search--header)))

(defun elfeed-time--set-sort-function (function)
  "Sort the elfeed-search buffer according to FUNCTION immediately."
  (setf elfeed-search-sort-function function)
  (elfeed-search-update--force))

(defun elfeed-time-sort-by-date ()
  "Sort the elfeed-search buffer by date."
  (interactive)
  (elfeed-time--set-sort-function nil))

(defun elfeed-time-sort-by-time ()
  "Sort the elfeed-search buffer by time to read entry."
  (interactive)
  (elfeed-time--set-sort-function #'elfeed-time-compare-entries))

(defun elfeed-time-toggle-sort-order ()
  "Reverse the order of the elfeed-search buffer."
  (interactive)
  (setf elfeed-sort-order (cl-case elfeed-sort-order
			    (ascending 'descending)
			    (descending 'ascending)))
  (elfeed-search-update--force))

(provide 'elfeed-time)
;;; elfeed-time.el ends here

