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

(defcustom helm-twitch-enable-livestreamer-actions t
  "Whether actions to use `livestreamer-mode' should be
available."
  :group 'helm-twitch
  :type 'boolean)

(defcustom helm-twitch-enable-chat-actions t
  "Whether actions to open Twitch chat should be available."
  :group 'helm-twitch
  :type 'boolean)

(defcustom helm-twitch-max-cache-age 60
  "Oldest permissable cache age for `helm-twitch' API calls, in
seconds."
  :group 'helm-twitch
  :type 'number)

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

(defun helm-twitch--livestreamer-header (stream)
  "Provides a Twitch.tv header for `livestreamer-mode'."
  (concat
   (propertize "Twitch.tv:" 'face 'font-lock-variable-name-face)
   " "
   (propertize (twitch-api-stream-name stream)
               'face 'helm-twitch-channel-face)
   " "
   (propertize (concat "(size: " livestreamer-current-size ")")
               'face 'font-lock-string-face)
   " -- "
   (propertize (format "%d viewers" (twitch-api-stream-viewers stream))
               'face 'helm-twitch-viewers-face)
   " -- "
   (propertize (twitch-api-stream-status stream)
               'face 'helm-twitch-status-face)))

(defun helm-twitch--livestreamer-open (stream)
  "Opens a Twitch.tv stream in `livestreamer-mode'."
  (let ((livestreamer-header-fn #'helm-twitch--livestreamer-header)
        (livestreamer-header-fn-args (list stream)))
    (livestreamer-open (twitch-api-stream-url stream))))

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

(defun helm-twitch--stream-candidates ()
  "Retrieve and format a list of stream that match whatever is
bound to HELM-PATTERN."
  (mapcar (lambda (stream) (cons (helm-twitch--format-stream stream) stream))
          (twitch-api-search-streams helm-pattern
                                     helm-twitch-candidate-number-limit)))

(defvar helm-twitch--stream-actions nil
  "Available `helm' actions for a stream.")

(defun helm-twitch--update-stream-actions ()
  "Updates available `helm' actions for a stream."
  (setq helm-twitch--stream-actions
        (append
         `(("Open this stream in a browser" .
            (lambda (stream) (browse-url (twitch-api-stream-url stream)))))
         (when helm-twitch-enable-livestreamer-actions
           `(("Open this stream in Livestreamer" .
              helm-twitch--livestreamer-open)))
         (when twitch-api-oauth-token
           `(("Follow this stream's channel" .
              (lambda (stream) (twitch-api-follow stream)))))
         (when helm-twitch-enable-chat-actions
           `(("Open Twitch chat for this channel" .
              (lambda (stream)
                (twitch-api-open-chat (twitch-api-stream-name stream)))))))))

(defvar helm-twitch--top-streams-cache nil)
(defvar helm-twitch--top-streams-cache-age nil)

(defun helm-twitch--top-stream-candidates ()
  "Retrieve and format a list of top live streams."
  (if (and helm-twitch--top-streams-cache
           (< (float-time (time-since helm-twitch--top-streams-cache-age))
              helm-twitch-max-cache-age))
      ;; Just use the cache, if it's new enough.
      helm-twitch--top-streams-cache
    ;; Otherwise we need to check again, and cache the result.
    (let ((top
           (mapcar (lambda (stream) (cons (helm-twitch--format-stream stream)
                                          stream))
                   (twitch-api-search-streams
                    "" helm-twitch-candidate-number-limit))))
      (setq helm-twitch--top-streams-cache-age (current-time)
            helm-twitch--top-streams-cache top)
      top)))

(defvar helm-twitch--following-cache nil)
(defvar helm-twitch--following-cache-age nil)

(defun helm-twitch--following-candidates ()
  "Retrieve and format a list of followed streams."
  (if (and helm-twitch--following-cache
           (< (float-time (time-since helm-twitch--following-cache-age))
              helm-twitch-max-cache-age))
      ;; Just use the cache, if it's new enough.
      helm-twitch--following-cache
    ;; Otherwise we need to check again, and cache the result.
    (let ((following
           (mapcar (lambda (stream) (cons (helm-twitch--format-stream stream)
                                          stream))
                   (twitch-api-get-followed-streams
                    helm-twitch-candidate-number-limit))))
      (setq helm-twitch--following-cache-age (current-time)
            helm-twitch--following-cache following)
      following)))

(defun helm-twitch--channel-candidates ()
  "Retrieve and format a list of channels that match whatever is
bound to HELM-PATTERN."
  (mapcar (lambda (channel) (cons (helm-twitch--format-channel channel) channel))
          (twitch-api-search-channels helm-pattern
                                      helm-twitch-candidate-number-limit)))

(defun helm-twitch--website-candidates ()
  "Format whatever is bound to HELM-PATTERN as a `helm' candidate
for searching Twitch.tv directly."
  (list (cons (concat (propertize "[?]" 'face 'helm-twitch-prefix-face)
                      " search "
                      (if (string= helm-pattern "")
                          "Twitch.tv directly"
                        (format "for `%s'" helm-pattern))
                      " in a browser")
              helm-pattern)))

(defun helm-twitch-flush-cache ()
  "Manually remove cached streams to force an update from the
Twitch.tv API."
  (setq helm-twitch--following-cache-age nil
        helm-twitch--following-cache nil
        helm-twitch--top-streams-cache-age nil
        helm-twitch--top-streams-cache nil))

(defvar helm-source-twitch
  (helm-build-sync-source "Live Streams"
    :header-name (lambda (src) (format "%s [%s]" src
                                       (or twitch-api-game-filter "All Games")))
    :volatile t
    :requires-pattern 2
    :init (lambda () (helm-twitch--update-stream-actions))
    :candidates #'helm-twitch--stream-candidates
    :action 'helm-twitch--stream-actions
    :persistent-help "Open this stream in a browser")
  "A `helm' source for Twitch streams.")

(defvar helm-source-twitch-top-streams
  (helm-build-sync-source "Top Live Streams"
    :header-name (lambda (src) (format "%s [%s]" src
                                       (or twitch-api-game-filter "All Games")))
    :volatile t
    :init (lambda () (helm-twitch--update-stream-actions))
    :candidates #'helm-twitch--top-stream-candidates
    :action 'helm-twitch--stream-actions
    :persistent-help "Open this stream in a browser")
  "A `helm' source for Twitch streams.")

(defvar helm-source-twitch-following
  (helm-build-sync-source "Live Followed Streams"
    :volatile t
    :candidates #'helm-twitch--following-candidates
    :action (helm-make-actions
             "Open this stream in a browser"
             (lambda (stream) (browse-url (twitch-api-stream-url stream)))
             "Open this stream in Livestreamer"
             'helm-twitch--livestreamer-open
             "Open Twitch chat for this channel"
             (lambda (stream)
               (twitch-api-open-chat (twitch-api-stream-name stream)))))
  "A `helm' source for Twitch streams the user is following.")

(defvar helm-source-twitch-channels
  (helm-build-sync-source "Channels"
    :volatile t
    ;; The Twitch.tv API seems to require at least three characters for channel
    ;; searches.
    :requires-pattern 3
    :candidates #'helm-twitch--channel-candidates
    :action (helm-make-actions
             "Open this channel"
             (lambda (channel) (browse-url (twitch-api-channel-url channel)))
             "Open Twitch chat for this channel"
             (lambda (channel)
               (twitch-api-open-chat (twitch-api-channel-name channel)))))
  "A `helm' source for Twitch channels.")

(defvar helm-source-twitch-website
  (helm-build-sync-source "Search Twitch.tv directly"
    :volatile t
    :candidates #'helm-twitch--website-candidates
    :action (helm-make-actions
             "Open the Twitch.tv website for this search"
             (lambda (query)
               (browse-url
                (concat "http://www.twitch.tv/search?query=" query)))))
  "A `helm' source for searching Twitch's website directly.")

;;;###autoload
(defun helm-twitch ()
  "Search for live Twitch.tv streams with `helm'."
  (interactive)
  (let* ((sources '(helm-source-twitch
                    helm-source-twitch-top-streams
                    helm-source-twitch-channels
                    helm-source-twitch-website))
         (sources (if twitch-api-oauth-token
                      (append '(helm-source-twitch-following)
                              sources)
                    sources)))
    (helm-other-buffer sources "*helm-twitch*")))

(provide 'helm-twitch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-twitch.el ends here
