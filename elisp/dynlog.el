;;; DYNLOG.EL --- : Enables use of a buffer to monitor live logs.

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	25 May 2004
;; Version: 	1.0
;; Keywords:	logs

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this program's
;; author (send electronic mail to <andyetitmoves@gmail.com>) or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; dynlog|R.Ramkumar|<andyetitmoves@gmail.com>
;; |: Enables use of a buffer to monitor live logs.
;; |$Date$|$Revision$|~/packages/dynlog.el

;;; Code:

(defgroup dynlog nil
  "*DynLog, a minor mode to to handle dynamic logs."
  :prefix "dynlog-"
  :group 'files)

(defvar dynlog-updation-lazy nil)
(defvar dynlog-updation-interval 1.0 "*")

(defvar dynlog-last-pos nil)
(defvar dynlog-update-timer nil)
(defvar dynlog-update-lock nil)
(defvar dynlog-visited-file nil)

;; Silence the compiler
(defvar dynlog-mode)

(eval-and-compile (defvar dynlog-debug nil))

(eval-and-compile
  (if dynlog-debug
      (defmacro make-func-timer (inter func lazy) t)
    (defsubst make-func-timer (inter func lazy)
      (funcall (if lazy 'run-with-idle-timer 'run-at-time) inter inter func))))

(defun dynlog-update-buffer ()
  (interactive)
  (if (and (not dynlog-update-lock) (boundp dynlog-mode) dynlog-mode)
      (unwind-protect
	  (progn
	    (setq dynlog-update-lock t)
	    (toggle-read-only -1)
	    (and buffer-file-name
		(if (string= buffer-file-name dynlog-visited-file)
		    (set-visited-file-name nil t)
		  (message "File being visited changed, disabling dynlog")
		  (setq dynlog-update-lock nil)
		  (setq dynlog-visited-file nil)
		  (dynlog-mode)))
	    (save-excursion
	      (goto-char (point-max))
	      (setq dynlog-last-pos
		    (if dynlog-last-pos
			(+ dynlog-last-pos
			   (cadr (insert-file-contents dynlog-visited-file
						       nil dynlog-last-pos)))
		      (+ (1- (point))
			 (cadr (insert-file-contents dynlog-visited-file
						     nil nil nil t))))))
	    (set-buffer-modified-p nil)
	    (toggle-read-only 1))
	(setq dynlog-update-lock nil))))

;;;###autoload
(define-minor-mode dynlog-mode
  "Minor mode to handle dynamic logs. Periodically updates a log being
constantly appended to, externally."
  nil " DynLog" nil :group 'dynlog
  (if dynlog-mode
      (unwind-protect
	  (progn
	    (or (and buffer-file-name (file-readable-p buffer-file-name))
		(error "Buffer must be associated with an readable file"))
	    (if (buffer-modified-p)
		(or (y-or-n-p "The buffer seems to be modified, any changes \
made would be lost. Do you want to continue? ")
		    (error "Aborted")))
	    (make-local-variable 'dynlog-last-pos)
	    (setq dynlog-last-pos nil)
	    (make-local-variable 'dynlog-update-timer)
	    (make-local-variable 'dynlog-update-lock)
	    (make-local-variable 'dynlog-visited-file)
	    (setq dynlog-update-timer
		  (make-func-timer dynlog-updation-interval
				   `(lambda () (and (eq (current-buffer)
							,(current-buffer))
						    (dynlog-update-buffer)))
				   dynlog-updation-lazy))
	    (setq dynlog-visited-file
		  (prog1
		      buffer-file-name
		    (set-visited-file-name nil t))))
	(unless dynlog-visited-file
	  (message "DynLog mode activation failed, resetting...")
	  (dynlog-mode)))
    (when dynlog-update-timer
      (cancel-timer dynlog-update-timer)
      (setq dynlog-update-timer nil))
    (when dynlog-visited-file
      (set-visited-file-name dynlog-visited-file t)
      (setq dynlog-visited-file nil))))

(provide 'dynlog)

;;; DYNLOG.EL ends here
