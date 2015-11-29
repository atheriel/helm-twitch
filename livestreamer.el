;;; livestreamer.el --- A major mode for Livestreamer output.

;; Copyright (C) 2015 Aaron Jacobs

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

(defcustom livestreamer-size "medium"
  "The stream size to request from Livestreamer."
  :type 'string)

(defcustom livestreamer-opts nil
  "Additional options to pass to Livestreamer."
  :type 'string)

(defvar-local livestreamer-process nil
  "The Livestreamer process for a `livestreamer-mode' buffer.")

(defun livestreamer-kill-buffer ()
  "Safely interrupt running stream players under Livestreamer
before killing the buffer."
  (interactive)
  (when (eq major-mode 'livestreamer-mode)
    (if (equal 'run (process-status livestreamer-process))
	(interrupt-process livestreamer-process)
      (kill-buffer))))

(defvar livestreamer-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'livestreamer-kill-buffer)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)))
  "Keymap for `livestreamer-mode'.")

(define-derived-mode livestreamer-mode fundamental-mode "livestreamer"
  "A major mode for Livestreamer output."
  :group 'helm-twitch
  (setq truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode))

(defun livestreamer--filter (proc output)
  "Filter OUTPUT from Livestreamer process PROC for display in
the buffer."
  (let ((buff (process-buffer proc)))
    (when (not (null buff))
      (with-current-buffer buff
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert output))))))

(defun livestreamer--sentinel (proc event)
  "Respond when Livestreamer process PROC receives EVENT."
  (let ((buff (process-buffer proc)))
    (when (not (null buff))
      (with-current-buffer buff
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (if (equal event "finished\n")
	      (insert (propertize "# Finished. Hit 'q' to kill the buffer."
				  'face 'font-lock-comment-face))
	    (insert (propertize (format "# Livestreamer process had event: %s"
					event)
				'face 'font-lock-comment-face))))))))

(defun livestreamer-open (url &optional size opts)
  "Opens the stream at URL using the Livestreamer program."
  (let* ((cmd  (executable-find "livestreamer"))
	 (size (or size livestreamer-size))
	 (opts (or opts livestreamer-opts ""))
	 (cmd  (when cmd (format "%s %s %s %s" cmd opts url size)))
	 (buff (when cmd (get-buffer-create "*livestreamer*"))))
    (if cmd
	(with-current-buffer buff
	  (switch-to-buffer buff)
	  (unless (eq major-mode 'livestreamer-mode)
	    (livestreamer-mode))
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert (propertize (format "# Livestreamer: %s\n" url)
				'face 'font-lock-comment-face))
	    (let ((proc (start-process-shell-command cmd buff cmd)))
	      (setq livestreamer-process proc)
	      (set-process-filter proc 'livestreamer--filter)
	      (set-process-sentinel proc 'livestreamer--sentinel))
	  nil))
      (message "Could not locate the livestreamer program."))))

(provide 'livestreamer)

;;; livestreamer.el ends here
