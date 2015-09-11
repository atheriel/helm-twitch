;;; helm-twitch.el --- Navigate Twitch.tv via `helm'.

;; Copyright (C) 2015 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; URL: https://github.com/atheriel/helm-twitch
;; Keywords: helm
;; Version: 0
;; Package-Requires: ((dash "2.11.0") (helm "1.5") (emacs "24"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use, just call M-x helm-twitch.

;;; Code:

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

(defcustom helm-twitch-username nil
  "A Twitch.tv username, for connecting to Twitch chat."
  :group 'helm-twitch
  :type 'string)

(defcustom helm-twitch-oauth-token nil
  "The OAuth token for the Twitch.tv username in `helm-twitch-username'.

To retrieve an OAuth token, check out `http://twitchapps.com/tmi/'."
  :group 'helm-twitch
  :type 'string)

(defun twitch--plist-to-url-params (plist)
  "Turn property list PLIST into an HTML parameter string."
  (mapconcat (lambda (entry)
	       (concat (url-hexify-string
			(nth 1 (split-string (format "%s" (nth 0 entry)) ":")))
		       "="
		       (url-hexify-string (format "%s" (nth 1 entry)))))
	     (-partition 2 plist) "&"))

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

(defun twitch-search-streams (search-term)
  "Retrieve a list of Twitch streams that match the SEARCH-TERM."
  (let ((results (if twitch-game-type
		     (twitch-api "streams" :query search-term :limit 10
				 :game twitch-game-type)
		   (twitch-api "streams" :query search-term :limit 10))))
    (alist-get '(streams) results)))

(defun twitch-search-channels (search-term)
  "Retrieve a list of Twitch channels that match the SEARCH-TERM."
  (alist-get '(channels)
	     (twitch-api "search/channels" :query search-term :limit 10)))

(defun helm-twitch-website-search (search-term)
  "Format SEARCH-TERM as a `helm' candidate for searching Twitch.tv directly."
  (list (cons (concat (propertize "[?]" 'face 'helm-twitch-prefix-face)
		      (format " search for `%s' in a browser" search-term))
	search-term)))

(defun twitch-api (endpoint &rest plist)
  "Query the Twitch API at ENDPOINT, returning the results as a Lisp structure.

Twitch API parameters can be passed in the property list PLIST.  For example:

    (twitch-api \"search/channels\" :query \"flame\" :limit 15)

This function does not perform error checking."
  (let* (;; TODO: Investigate using `url-request-data' instead.
	 (params (twitch--plist-to-url-params plist))
	 (api-url (concat "https://api.twitch.tv/kraken/" endpoint "?" params))
	 ;; Use version 3 of the API.
	 (url-request-extra-headers
	  '(("Accept" . "application/vnd.twitchtv.v3+json")))
	 )
    (kill-new api-url) 			; For debugging.
    (with-current-buffer
	(url-retrieve-synchronously api-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun helm-twitch-open-chat (channel-name)
  "Invokes `erc' to open Twitch chat for a given CHANNEL-NAME."
  (interactive "sChannel: ")
  (if (and helm-twitch-username helm-twitch-oauth-token)
      (progn
	(erc :server "irc.twitch.tv" :port 6667
	     :nick (downcase helm-twitch-username)
	     :password helm-twitch-oauth-token)
	(erc-join-channel (format "#%s" (downcase channel-name))))
    (when (not helm-twitch-username)
      (message "Set the variable `helm-twitch-username' to connect to Twitch chat."))
    (when (not helm-twitch-oauth-token)
      (message "Set the variable `helm-twitch-oauth-token' to connect to Twitch chat."))))

(defvar helm-source-twitch
  '((name . "Live Streams")
    (volatile)
    (candidates-process
     . (lambda ()
	 ;; Format the list of returned streams.
	 (mapcar (lambda (stream) (cons (twitch-format-stream stream) stream))
		 (twitch-search-streams helm-pattern))))
    (action . (("Open this stream"
		. (lambda (stream)
		    (browse-url (alist-get '(url) (alist-get '(channel) stream)))))
	       ("Open Twitch chat for this channel"
		. (lambda (stream)
		    (helm-twitch-open-chat
		     (alist-get '(name) (alist-get '(channel) stream)))))
	       )))
  "A `helm' source for Twitch streams.")

(defvar helm-source-twitch-channels
  '((name . "Channels")
    (volatile)
    ;; The Twitch.tv API seems to require at least three characters for channel
    ;; searches.
    (requires-pattern . 3)
    (candidates-process
     . (lambda ()
	 ;; Format the list of returned channels.
	 (mapcar (lambda (channel) (cons (twitch-format-channel channel) channel))
		 (twitch-search-channels helm-pattern))))
    (action . (("Open this channel"
		. (lambda (stream) (browse-url (alist-get '(url) stream))))
	       ("Open Twitch chat for this channel"
		. (lambda (channel)
		    (helm-twitch-open-chat (alist-get '(name) channel))))
	       )))
  "A `helm' source for Twitch channels.")

(defvar helm-source-twitch-website
  '((name . "Search Twitch.tv directly")
    (volatile)
    ;; Require two letters (the smallest number there may be no results for),
    ;; so that it does not need to show up in the initial buffer.
    (requires-pattern . 2)
    (candidates-process . (lambda () (helm-twitch-website-search helm-pattern)))
    (action . (("Open the Twitch.tv website with this search term"
		. (lambda (query)
		    (browse-url (concat "http://www.twitch.tv/search?query="
					query))))
	       )))
  "A `helm' source for searching Twitch's website directly.")

(defun helm-twitch ()
  "Search for live Twitch.tv streams with `helm'."
  (interactive)
  (helm-other-buffer '(helm-source-twitch
		       helm-source-twitch-channels
		       helm-source-twitch-website)
		     "*helm-twitch*"))

(provide 'helm-twitch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-twitch.el ends here
