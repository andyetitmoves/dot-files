;;; SHELL-CONVERT.EL --- Convert a file using a shell command and then display it.

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	14 Jul 2004
;; Version: 	1.0
;; Keywords:	shell

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
;; shell-convert|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Convert a file using a shell command and then display it.
;; |$Date: 2004/07/14 16:49:53 $|$Revision: 1.1 $|~/packages/shell-convert.el

(defconst default-conv-flags  '())
(defconst default-conv-io-args (list "%s" "-"))

(defvar conv-param-list (list)
  "Parameters to use for conversion of buffer by the specified command.
Each entry in the list is of form (REGPRED COMMAND OPTIONAL_FLAGS
OPTIONAL_IO_PARAMS) REGPRED is the regular expression used to validate the
filename used with the COMMAND specified.  Alternatively, REGPRED can be a
predicate to which, if the filename is passed, non-nil is the return on
successful validation.  OPTIONAL_FLAGS is the optional list of flags to be
passed to the command executed.  OPTIONAL_IO_PARAMS is the list of parameters to
specify the input file and standard output as the output file.  Use %s as a
placeholder for the buffer filename. %% represents the literal '%' character.
The default value is (\"%s\" \"-\").")

(defvar conv-file-assoc nil
  "The status of the file after filling the buffer with the converted text.
The default nil dissociates the buffer from the original file.  d lets the
buffer retain its original value but does not save the contents.  Any other
string saves the file, effectively converting the original file to the format as
got by the command output.")

(defvar conv-lock-on-replace t
  "Non-nil, if the file is made read-only on display of the contents.")

(eval-and-compile
  (require 'working nil t)

  (or (fboundp 'working-status-timeout)
      (defmacro working-status-timeout (&rest args)
	`(progn ,@args))))

(defun map-till-t (seq func)
  (if (car seq)
      (if (not (eval (list func '(car seq))))
	  (map-till-t (cdr seq) func)
	seq)
    nil))

(defun conv-check-param (thisp)
  (if (listp thisp)
      (let ((check (car thisp)) (case-fold-search nil))
	(if check
	    (cond
	     ((stringp check) (string-match (car thisp) (buffer-file-name)))
	     ((functionp check) (funcall check (buffer-file-name)))
	     (t (progn
		  (message "shell-conv: Warning: Non-string or predicate used \
for filename validation")nil)))))
    (message "shell-conv: Warning: A member of the conv-param-list is not a \
list, please refer documentation for details.")
    nil))

(defun conv-compose-arg (arg-tmpl)
  (replace-regexp-in-string
   "%%" "%" (replace-regexp-in-string "%s" (buffer-file-name) arg-tmpl nil t)))

(defun conv-shell-output-filter (pobj outstr)
  (save-excursion
    (goto-char (point-max))
    (insert outstr)))

(defun conv-successful-file-finish ()
  (if conv-file-assoc
      (if (not (and (stringp conv-file-assoc) (= conv-file-assoc "d")))
	  (save-buffer))
    (progn
      (delete-auto-save-file-if-necessary t)
      (auto-save-mode nil)
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil)))
  (if conv-lock-on-replace
      (toggle-read-only t))
  nil)

(defun conv-fail-file-finish()
  (if (y-or-n-p "All valid conversion commands failed, \
do you want to display the original text ")
      (progn
	(delete-region 1 (point-max))
	(insert-file (buffer-file-name))
	(set-buffer-modified-p nil))
    (progn
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun eval-dbg (stat)
  (print stat t)
  (eval stat))

(defun shell-conv-find-file-hook ()
  "The function to be called by the find-file hook for formatting
text using a shell command before displaying them."
  (if (not (buffer-file-name))
      (error "shell-conv: A buffer file name is needed for conversion"))
  (let ((match conv-param-list) (flag 4) elt pobj)
    (while (and flag (setq match (map-till-t match 'conv-check-param)))
      (setq flag 3)
      (setq elt (cdar match))
      (if (not elt)
	  (message "shell-conv: No command has specified for matching \
association of %s , ignoring..." (car (car match)))
	(if (cdr elt)
	    (if (not (cddr elt)) (nconc elt (list default-conv-io-args)))
	  (nconc elt (list default-conv-flags) (list default-conv-io-args)))
	(if (not buffer-read-only)
	    (setq flag 2)
	  (toggle-read-only)
	  (setq flag 1))
	(delete-region 1 (point-max) )
	(if (setq pobj (eval (append (list 'start-process "conv-proc" nil
					   (conv-compose-arg (car elt)))
				     (mapcar 'conv-compose-arg (cadr elt))
				     (mapcar 'conv-compose-arg
					     (car (cddr elt))))))
	    (let (exits)
	      (set-process-filter pobj 'conv-shell-output-filter)
	      (working-status-timeout .1 "Converting..." "Done"
		(while (eq (process-status pobj) 'run)
		  (accept-process-output pobj))
		(accept-process-output pobj))
	      (if (or (nth 3 elt) (= (setq exits (process-exit-status pobj)) 0))
		  (setq flag (conv-successful-file-finish))
		(message "shell-conv: Process %s failed with exit code %d"
			 (car elt) exits)))
	  (message "shell-conv: Unable to spawn process to convert file")
	  (if (= flag 1)(toggle-read-only))))
      (setq match (cdr match)))
    (if (and flag (<= flag 2)) (conv-fail-file-finish))))

(provide 'shell-convert)

;;; SHELL-CONVERT.EL ends here
