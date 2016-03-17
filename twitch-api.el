;;; twitch-api.el --- An elisp interface for the Twitch.tv API

;; Copyright (C) 2015-2016 Aaron Jacobs

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

;;; Code:

(require 'url)
(require 'dash)
(require 'json)

(defcustom twitch-api-game-filter "League of Legends"
  "If specified, limits the search to those streaming this game."
  :version 0.1
  :type 'string)

(defcustom twitch-api-username nil
  "A Twitch.tv username, for connecting to Twitch chat."
  :group 'helm-twitch
  :type 'string)

(defcustom twitch-api-oauth-token nil
  "The OAuth token for the Twitch.tv username in `twitch-api-username'.

To retrieve an OAuth token, check out `http://twitchapps.com/tmi/'."
  :group 'helm-twitch
  :type 'string)

;;;; Utilities

(defun twitch-api--plist-to-url-params (plist)
  "Turn property list PLIST into an HTML parameter string."
  (mapconcat (lambda (entry)
	       (concat (url-hexify-string
			(nth 1 (split-string (format "%s" (nth 0 entry)) ":")))
		       "="
		       (url-hexify-string (format "%s" (nth 1 entry)))))
	     (-partition 2 plist) "&"))

;;;; Data Structures

(cl-defstruct (twitch-api-stream (:constructor twitch-api-stream--create))
  "A Twitch.tv stream."
  name viewers status game url)

(cl-defstruct (twitch-api-channel (:constructor twitch-api-channel--create))
  "A Twitch.tv channel."
  name followers game url)

;;;; API Wrappers

(defun twitch-api (endpoint &rest plist)
  "Query the Twitch API at ENDPOINT, returning the resulting JSON
in a property list structure.

Twitch API parameters can be passed in the property list PLIST.
For example:

    (twitch-api \"search/channels\" :query \"flame\" :limit 15)
"
  (let* (;; TODO: Investigate using `url-request-data' instead.
	 (params (twitch-api--plist-to-url-params plist))
	 (api-url (concat "https://api.twitch.tv/kraken/" endpoint "?" params))
	 ;; Decode into a plist, not the default alist.
	 (json-object-type 'plist)
	 ;; Use version 3 of the API.
	 (url-request-extra-headers
	  '(("Accept" . "application/vnd.twitchtv.v3+json")))
	 )
    ;; (kill-new api-url) 			; For debugging.
    (with-current-buffer
      (url-retrieve-synchronously api-url t)
      (setq coding-system 'utf-8)
      (goto-char url-http-end-of-headers)
      (let ((result (json-read)))
	(when (plist-get result ':error)
	  ;; According to the Twitch API documentation, the JSON object should
	  ;; contain error information of this kind on failure:
	  (user-error "Twitch.tv API request failed: %d (%s) %s"
		      (plist-get result ':status)
		      (plist-get result ':error)
		      (concat (when (plist-get result ':message)
				(concat " - " (plist-get result ':message))))))
	result))))

(defun twitch-api-search-streams (search-term)
  "Retrieve a list of Twitch streams that match the SEARCH-TERM."
  (let ((results (if twitch-api-game-filter
		     (twitch-api "streams" :query search-term :limit 10
				 :game twitch-api-game-filter)
		   (twitch-api "streams" :query search-term :limit 10))))
    (cl-loop for stream across (plist-get results ':streams) collect
	     (let ((channel (plist-get stream ':channel)))
	       (twitch-api-stream--create
		:name    (plist-get channel ':name)
		:viewers (plist-get stream ':viewers)
		:status  (plist-get channel ':status)
		:game    (plist-get channel ':game)
		:url     (plist-get channel ':url))))))

(defun twitch-api-search-channels (search-term)
  "Retrieve a list of Twitch channels that match the SEARCH-TERM."
  (let ((results (twitch-api "search/channels"
			     :query search-term :limit 10)))
    (cl-loop for channel across (plist-get results ':channels) collect
	     (twitch-api-channel--create
	      :name      (plist-get channel ':name)
	      :followers (plist-get channel ':followers)
	      :game      (plist-get channel ':game)
	      :url       (plist-get channel ':url)))))

;;;; Twitch Chat Interaction

(defun twitch-api-open-chat (channel-name)
  "Invokes `erc' to open Twitch chat for a given CHANNEL-NAME."
  (interactive "sChannel: ")
  (if (and twitch-api-username twitch-api-oauth-token)
      (progn
	(require 'erc)
	(erc :server "irc.twitch.tv" :port 6667
	     :nick (downcase twitch-api-username)
	     :password twitch-api-oauth-token)
	(erc-join-channel (format "#%s" (downcase channel-name))))
    (when (not twitch-api-username)
      (message "Set the variable `twitch-api-username' to connect to Twitch chat."))
    (when (not twitch-api-oauth-token)
      (message "Set the variable `twitch-api-oauth-token' to connect to Twitch chat."))))

(provide 'twitch-api)

;; Local Variables:
;; coding: utf-8
;; End:

;;; twitch-api.el ends here
