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

(defcustom livestreamer-header-fn #'livestreamer-default-header
  "The function used to set the header-line."
  :type 'function)

(defcustom livestreamer-header-fn-args nil
  "Arguments to pass `livestreamer-header-fn'."
  :type 'list)

(defvar-local livestreamer-process nil
  "The Livestreamer process for a `livestreamer-mode' buffer.")

(defvar-local livestreamer-url nil
  "The current stream URL for a `livestreamer-mode' buffer.")

(defvar-local livestreamer-current-size nil
  "The current stream size for a `livestreamer-mode' buffer.")

(defun livestreamer-default-header ()
  "Provides the default header for `livestreamer-mode'."
  (concat
   (propertize "Livestreamer:" 'face 'font-lock-variable-name-face) " "
   (propertize livestreamer-url 'face 'font-lock-constant-face) " "
   (propertize (concat "(" livestreamer-current-size ")")
	       'face 'font-lock-string-face)))

(defun livestreamer-kill-buffer ()
  "Safely interrupt running stream players under Livestreamer
before killing the buffer."
  (interactive)
  (when (eq major-mode 'livestreamer-mode)
    (if (equal 'run (process-status livestreamer-process))
	(interrupt-process livestreamer-process)
      (kill-buffer))))

(defun livestreamer-reopen-stream ()
  "Re-open the previous stream for this buffer using
Livestreamer."
  (interactive)
  (when (eq major-mode 'livestreamer-mode)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (if (equal 'run (process-status livestreamer-process))
	  ;; Don't try and re-open if the stream ain't closed!
	  (insert (propertize
		   "# Cannot re-open stream: a stream is still open.\n"
		   'face 'font-lock-comment-face))
	(insert (propertize "# Re-opening stream...\n"
			    'face 'font-lock-comment-face))
	(livestreamer-open livestreamer-url nil nil 'no-erase)))))

(defun livestreamer--get-stream-sizes ()
  "Retrieve the available stream sizes from the buffer's process
output content."
  (save-excursion
    (with-current-buffer "*livestreamer*"
      (goto-char (point-min))
      (let* ((start (progn
		      (re-search-forward "Available streams:\s")
		      (point)))
	     (end (progn (re-search-forward "$") (point))))
	(split-string
	 (s-replace " (worst)" ""
		    (s-replace " (best)" ""
			       (buffer-substring start end))) ", ")))))

(defun livestreamer-resize-stream (size)
  "Re-open the stream with a different size."
  (interactive
   (list (intern (completing-read
		  "Size: "
		  (livestreamer--get-stream-sizes) nil t))))
  (when (eq major-mode 'livestreamer-mode)
    (when (and (equal 'run (process-status livestreamer-process))
	       (y-or-n-p "Stream is currently open. Close it? "))
      (interrupt-process livestreamer-process))
    (livestreamer-open livestreamer-url size nil 'no-erase)))

(defvar livestreamer-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'livestreamer-kill-buffer)
      (define-key map "r" 'livestreamer-reopen-stream)
      (define-key map "s" 'livestreamer-resize-stream)
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
	      (insert
	       (propertize
		"# Finished. Hit 'q' to kill the buffer or 'r' to re-open.\n"
		'face 'font-lock-comment-face))
	    (insert (propertize (format "# Livestreamer process had event: %s\n"
					event)
				'face 'font-lock-comment-face))))))))

(defun livestreamer-open (url &optional size opts no-erase)
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
	    (when (not no-erase)
	      (erase-buffer)
	      (insert (propertize (format "# Livestreamer: %s\n" url)
				  'face 'font-lock-comment-face)))
	    (let ((proc (start-process-shell-command cmd buff cmd)))
	      (setq livestreamer-process proc
		    livestreamer-url url
		    livestreamer-current-size size
		    header-line-format
		    `(:eval (funcall ',livestreamer-header-fn
				     ,@livestreamer-header-fn-args)))
	      (set-process-filter proc 'livestreamer--filter)
	      (set-process-sentinel proc 'livestreamer--sentinel))
	  nil))
      (message "Could not locate the livestreamer program."))))

(provide 'livestreamer)

;;; livestreamer.el ends here
