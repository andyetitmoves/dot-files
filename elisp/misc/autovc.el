;;; AUTOVC.EL --- Warns about edited and checked out packages at emacs exit

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	09 May 2004
;; Version: 	1.0
;; Keywords:	vc

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Code:

(defvar autovc-checkout-on-change 'ask)
(defvar autovc-checkin-on-exit 'ask)

(defvar autovc-buffer-list nil)
(defvar autovc-suspend-kill nil)

(require 'vc)

;;;###autoload
(defun autovc-find-file-hook ()
  (when (and (vc-backend (buffer-file-name)) (not autovc-suspend-kill))
    (add-to-list 'kill-emacs-query-functions 'autovc-on-query-emacs-kill)
    (add-to-list 'autovc-buffer-list (buffer-file-name))))

(defun autovc-cleanup-hooks ()
  "Cleans up hooks set up to resume exit if suspension is needed"
  (let (logbuf)
    (message "Temporary suspension of exit no longer needed")
    (remove-hook 'vc-checkin-hook 'autovc-on-log-over)
    (if (setq logbuf (get-buffer "*VC-log*"))
	(remove-hook kill-buffer-hook 'autovc-on-log-over t))
    (setq autovc-suspend-kill nil)))

(defun autovc-on-log-over ()
  "Hook called to continue exit if suspended for logging"
  (when autovc-suspend-kill
    (autovc-cleanup-hooks)
    (save-buffers-kill-emacs)))

(defun autovc-on-query-emacs-kill ()
  "Hook called on emacs exit to determine if there are any checked out packages"
  (and autovc-suspend-kill
       ;; exit has been called while taking (possibly checkin) action on a file.
       ;; so, just ignore the current file, and move on to the next one.
       (autovc-cleanup-hooks))
  (while (and (not autovc-suspend-kill) autovc-buffer-list)
    (autovc-take-action (car autovc-buffer-list))
    (and (not autovc-suspend-kill)
	 (setq autovc-buffer-list (cdr autovc-buffer-list))))
  (or (not autovc-suspend-kill)
      ;; ok, interrupt the exit to accomodate logging.
      (let (logbuf)
	(message "Temporarily aborting exit to accomodate log message")
	;; hey, this is just a temporary abort.
	;; come back after logging is over!
	(add-hook 'vc-checkin-hook 'autovc-on-log-over)
	(setq autovc-suspend-kill autovc-buffer-list)
	(and (setq logbuf (get-buffer "*VC-log*"))
	     (with-current-buffer logbuf
	       ;; the user may abort the checkin and close the log buffer.
	       (add-hook kill-buffer-hook 'autovc-on-log-over nil t)))
	;; signal "no exit"
	nil)))

;; almost the same as vc-next-action, but the original version didn't suit me.
;; so, just changed that a bit and put it over here...
(defun autovc-take-action (file)
  (let ((visited (get-file-buffer file)) state version err)
    (when visited
      (if vc-dired-mode
	  (find-file-other-window file)
	(set-buffer (find-file-noselect file)))
      (if (not (verify-visited-file-modtime (current-buffer)))
	  (and (y-or-n-p "Replace file on disk with buffer contents? ")
	       (write-file (buffer-file-name)))
	(vc-buffer-sync t)
	(and (buffer-modified-p)
	     (or (y-or-n-p "Operate on disk file, keeping modified buffer? ")
		 (setq err t)))))
    (unless (or err (not (vc-registered file)))
      (vc-recompute-state file)
      (if visited (vc-mode-line file))
      (setq state (vc-state file))
      (cond
       ((and visited (eq state 'edited)
	     buffer-read-only (not (file-writable-p file)))
	(message "File is edited but read-only; making it writable")
	(set-file-modes buffer-file-name
			(logior (file-modes buffer-file-name) 128))
	(toggle-read-only -1)
	(autovc-take-action file))
       ((eq state 'edited)
	(cond
	 ((and (not (eq (vc-checkout-model file) 'implicit))
	       (vc-workfile-unchanged-p file)
	       (not (and visited (buffer-modified-p))))
	  (if (not visited) (find-file-other-window file))
	  (if (y-or-n-p "Locked file is unchanged, revert to master version? ")
	      (vc-revert-buffer)))
	 (t
	  (when (y-or-n-p (format "The file %s has changed after checkout, \
commit changes? " (file-name-nondirectory file)))
	    (vc-checkin file nil nil)
	    (setq autovc-suspend-kill t)))))
       ((eq state 'needs-merge)
	(and (y-or-n-p (format "%s is not up-to-date. Merge in changes now? "
			       (file-name-nondirectory file)))
	     (vc-maybe-resolve-conflicts file (vc-call merge-news file))))
       ((eq state 'unlocked-changes)
	(and (not visited) (find-file-other-window file))
	(when (save-window-excursion
		(vc-version-diff file (vc-workfile-version file) nil)
		(goto-char (point-min))
		(let ((inhibit-read-only t))
		  (insert (format "Changes to %s since last lock:\n\n" file)))
		(not (beep))
		(y-or-n-p (concat "File has unlocked changes.  "
				  "Claim lock retaining changes? ")))
	  (vc-call steal-lock file)
	  (vc-clear-headers file)
	  (vc-mode-line file)
	  (autovc-take-action file)))))))

(provide 'autovc)

;;; AUTOVC.EL ends here
