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

(require 'helm)

(require 'twitch-api)
(require 'livestreamer)

(defgroup helm-twitch nil
  "A helm plugin to search for live Twitch channels."
  :group 'convenience)

(defcustom helm-twitch-candidate-number-limit 25
  "The limit on the number of candidates `helm-twitch' will
return from a single source. Note that there are significant
performance costs to increasing this beyond 100 or so.

Similar to `helm-candidate-number-limit'."
  :group 'helm-twitch
  :type 'string)

(defface helm-twitch-prefix-face
    '((t (:inherit 'helm-ff-prefix)))
  "Face used to prefix the search query in `helm-twitch'."
  :group 'helm-twitch)

(defface helm-twitch-channel-face
    '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face used for streamer/channel names in `helm-twitch'."
  :group 'helm-twitch)

(defface helm-twitch-viewers-face
    '((t (:inherit font-lock-string-face)))
  "Face used for viewer/follower count in `helm-twitch'."
  :group 'helm-twitch)

(defface helm-twitch-status-face
    '((t (:inherit font-lock-comment-face)))
  "Face used for a stream's status in `helm-twitch'."
  :group 'helm-twitch)

(defun helm-twitch--format-stream (stream)
  "Given a `twitch-api-stream' STREAM, return a a formatted string
suitable for display in a *helm-twitch* buffer."
  (let* ((viewers (format "%7d" (twitch-api-stream-viewers stream)))
	 (name    (format "%-16s" (twitch-api-stream-name stream)))
	 (status (truncate-string-to-width
		  (twitch-api-stream-status stream) 45)))
    (concat (propertize name 'face 'helm-twitch-channel-face)
	    "  "
	    (propertize (concat viewers " viewers")
			'face 'helm-twitch-viewers-face)
	    "  "
	    (propertize status 'face 'helm-twitch-status-face))))

(defun helm-twitch--format-channel (channel)
  "Given a `twitch-api-channel' CHANNEL, return a a formatted string
suitable for display in a *helm-twitch* buffer."
  (let* ((followers (format "%7d" (twitch-api-channel-followers channel)))
	 (name      (format "%-16s" (twitch-api-channel-name channel)))
	 (game      (format "%s" (or (twitch-api-channel-game channel) ""))))
    (concat (propertize name 'face 'helm-twitch-channel-face)
	    "  "
	    (propertize (concat followers " followers")
			'face 'helm-twitch-viewers-face)
	    "  "
	    (propertize game 'face 'helm-twitch-status-face))))

(defun helm-twitch-website-search (search-term)
  "Format SEARCH-TERM as a `helm' candidate for searching Twitch.tv directly."
  (list (cons (concat (propertize "[?]" 'face 'helm-twitch-prefix-face)
		      (format " search for `%s' in a browser" search-term))
	search-term)))

(defvar helm-source-twitch
  '((name . "Live Streams")
    (volatile)
    (candidates-process
     . (lambda ()
	 ;; Format the list of returned streams.
	 (mapcar (lambda (stream) (cons (helm-twitch--format-stream stream) stream))
		 (twitch-api-search-streams helm-pattern
					    helm-twitch-candidate-number-limit))))
    (action . (("Open this stream in a browser"
		. (lambda (stream)
		    (browse-url (twitch-api-stream-url stream))))
	       ("Open this stream in Livestreamer"
		. (lambda (stream)
		    (livestreamer-open (twitch-api-stream-url stream))))
	       ("Open Twitch chat for this channel"
		. (lambda (stream)
		    (twitch-api-open-chat (twitch-api-stream-name stream))))
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
	 (mapcar (lambda (channel) (cons (helm-twitch--format-channel channel) channel))
		 (twitch-api-search-channels helm-pattern
					     helm-twitch-candidate-number-limit))))
    (action . (("Open this channel"
		. (lambda (channel) (browse-url (twitch-api-channel-url channel))))
	       ("Open Twitch chat for this channel"
		. (lambda (channel)
		    (twitch-api-open-chat (twitch-api-channel-name channel))))
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
