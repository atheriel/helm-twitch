;;; helm-twitch --- Navigate twitch.tv via Helm

(require 'url)
(require 'dash)
(require 'json)
(require 'helm)

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defgroup helm-twitch nil
  "A helm plugin to search for live Twitch channels."
  :group 'convenience)

(defface helm-twitch-prefix-face
    '((t (:inherit 'helm-ff-prefix)))
  "Face used to prefix the search query in `helm-twitch'."
  :group 'helm-twitch)

(defface helm-twitch-streamer-face
    '((t (:background "#3F3F3F" :foreground "#8CD0D3")))
  "Face used to prefix new file or url paths in `helm-find-files'."
  :group 'helm-twitch)

(defface helm-twitch-viewers-face
    '((t (:background "#3F3F3F" :foreground "#F0DFAF")))
  "Face used to prefix new file or url paths in `helm-find-files'."
  :group 'helm-twitch)

(defface helm-twitch-status-face
    '((t (:background "#3F3F3F" :foreground "#7F9F7F")))
  "Face used to prefix new file or url paths in `helm-find-files'."
  :group 'helm-twitch)

(defcustom twitch-game-type "League of Legends"
  "If specified, limits the search to those streaming this game."
  :version 0.1
  :type 'string)

(defcustom twitch-api-client-id nil
  ""
  :version 0.1
  :type 'string)

(defun twitch-search (search-term)
  "Search Twitch for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (if (not twitch-game-type)
		   (format "https://api.twitch.tv/kraken/streams?q=%s&limit=10" search-term)
		 (format "https://api.twitch.tv/kraken/streams?game=%s&q=%s&limit=10"
			 twitch-game-type search-term)))
	(url-request-extra-headers
	 (and twitch-api-client-id
	      '(("Client-ID" . twitch-api-client-id))))
	)
    ;; (warn a-url)
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun twitch-format-stream (stream)
  "Given a STREAM, return a a formatted string suitable for display."
  (let* ((viewers (format "%6s" (alist-get '(viewers) stream)))
	 (name    (format "%-20s" (alist-get '(name)
					     (alist-get '(channel) stream))))
	 (raw-status (format "%s" (alist-get '(status)
					     (alist-get '(channel) stream))))
	 (status (if (> (length raw-status) 37)
		     ;; Truncate the status if it's too long.
		     (format "%s..."
			     (substring raw-status 0 (min (length raw-status) 36)))
		   raw-status))
	 )
    (concat (propertize name 'face 'helm-twitch-streamer-face)
	    "  "
	    (propertize (concat viewers " viewers")
			'face 'helm-twitch-viewers-face)
	    "  "
	    (propertize status 'face 'helm-twitch-status-face))))

(defun twitch-search-formatted (search-term)
  (mapcar (lambda (stream)
	    (cons (twitch-format-stream stream) stream))
	  (alist-get '(streams) (twitch-search search-term))))

;; (twitch-search-formatted "fro")

(defun helm-twitch-search ()
  (twitch-search-formatted helm-pattern))

(defvar helm-source-twitch
  '((name . "Live Streams")
    (volatile)
    ;; (delayed)
    ;; (requires-pattern . 2)
    (candidates-process . helm-twitch-search)
    (action . (("Open this stream"
		. (lambda (stream) (browse-url (alist-get '(name) stream))))
	       )))
  "A `helm' source for Twitch streams.")

(defun helm-twitch-website-search ()
  "Returns a formatted `helm' candidate for searching twitch.tv directly."
  (list (cons (concat (propertize "[?]" 'face 'helm-twitch-prefix-face)
		      (format " search for `%s' in a browser" helm-pattern))
	helm-pattern)))

(defvar helm-source-twitch-website
  '((name . "Search twitch.tv directly")
    (volatile)
    ;; Require two letters (the smallest number there may be no results for),
    ;; so that it does not need to show up in the initial buffer.
    (requires-pattern . 2)
    (candidates-process . helm-twitch-website-search)
    (action . (("Open the twitch.tv website with this search term"
		. (lambda (query)
		    (browse-url (concat "http://www.twitch.tv/search?query="
					query))))
	       )))
  "A `helm' source for searching Twitch's website directly.")

(defun twitch--search (search-term kind &optional limit)
  "Search Twitch for SEARCH-TERM, returning the results as a Lisp structure."
  (let* ((lim (format "&limit=%s" (or limit 10)))
	 ;; The `game' parameter seems to have no effect in channel searches.
	 (game (and twitch-game-type
		    (format "&game=%s" (url-encode-url twitch-game-type))))
	 (api-url (concat "https://api.twitch.tv/kraken/search/" kind
			  "?q=" search-term
			  lim game)))
    (kill-new api-url)
    (with-current-buffer
	(url-retrieve-synchronously api-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun twitch-format-channel (channel)
  "Given a CHANNEL, return a a formatted string suitable for display."
  (let* ((followers (format "%6s" (alist-get '(followers) channel)))
	 (name      (format "%-20s" (alist-get '(name) channel)))
	 (game      (format "%s" (alist-get '(game) channel))))
    (concat (propertize name 'face 'helm-twitch-streamer-face)
	    "  "
	    (propertize (concat followers " followers")
			'face 'helm-twitch-viewers-face)
	    "  "
	    (propertize game 'face 'helm-twitch-status-face))))

(defun twitch-search-channels (search-term)
  (mapcar (lambda (channel)
	    (cons (twitch-format-channel channel) channel))
	  (alist-get '(channels) (twitch--search search-term "channels"))))

(defvar helm-source-twitch-channels
  '((name . "Channels")
    (volatile)
    ;; The twitch.tv API seems to require at least three characters for channel
    ;; searches.
    (requires-pattern . 3)
    (candidates-process . (lambda () (twitch-search-channels helm-pattern)))
    (action . (("Open this channel"
		. (lambda (stream) (browse-url (alist-get '(url) stream))))
	       )))
  "A `helm' source for Twitch channels.")

(defun helm-twitch ()
  "Search for live Twitch streams with `helm'."
  (interactive)
  (helm-other-buffer '(helm-source-twitch
		       helm-source-twitch-channels
		       helm-source-twitch-website)
		     "*helm-twitch*"))

(provide 'helm-twitch)
