;;; PROJ.EL --- C/C++ working project description interface

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	31 Oct 2004
;; Version: 	1.0
;; Keywords:	c c++ project

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
;; author (send electronic mail to andyetitmoves@gmail.com) or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; proj|R.Ramkumar|andyetitmoves@gmail.com
;; |C/C++ working project description interface
;; |$Date$|$Revision$|~/packages/proj.el

;;; Code:

(defcustom proj-toplevel (expand-file-name "~")
  "Top level of working project"
  :type 'directory)

(defcustom proj-name ""
  "Name of working project"
  :type 'string)

(defcustom proj-description ""
  "Description of working project"
  :type 'string)

(defcustom proj-use-relative-paths nil
  "Use relative paths for header file substitutions"
  :type 'boolean)

(defcustom proj-c++ nil
  "The project is in C++"
  :type 'boolean)

(defcustom proj-library nil
  "The project is a library"
  :type 'boolean)

(defun proj-set-variable (var prompt)
  (let* ((minibuffer-help-form '(describe-variable var))
	 (val
	  (let ((prop (get var 'variable-interactive))
		(type (get var 'custom-type)))
	    (unless (listp type)
	      (setq type (list type)))
	    (cond (prop
		   ;; Use VAR's `variable-interactive' property
		   ;; as an interactive spec for prompting.
		   (call-interactively (list 'lambda '(arg)
					     (list 'interactive prop)
					     'arg)))
		  (type
		   (widget-prompt-value type
					prompt
					(if (boundp var)
					    (symbol-value var))
					(not (boundp var))))
		  (t
		   (eval-minibuffer prompt))))))
    (customize-set-variable var val)))

;;;###autoload
(defun proj-set ()
  (interactive)
  (proj-set-variable 'proj-toplevel "Toplevel directory")
  (proj-set-variable 'proj-name "Name: ")
  (proj-set-variable 'proj-description "Description: ")
  (proj-set-variable 'proj-use-relative-paths
		     "Use relative paths for header ifdefs ? ")
  (proj-set-variable 'proj-c++
		     "Is the project in C++ ? ")
  (proj-set-variable 'proj-library
		     "Is the project a library ? ")
  (customize-save-customized))

(defvar proj-header-include-once)
(put 'proj-header-include-once 'permanent-local t)

(defun proj-make-header-include-once-string ()
  (set (make-local-variable 'proj-header-include-once)
       (let ((base (replace-regexp-in-string
		    "[^a-zA-Z0-9_]" "_"
		    (if (boundp 'proj-toplevel)
			(file-relative-name buffer-file-name proj-toplevel)
		      (file-name-nondirectory buffer-file-name)) t t)))
	 (and proj-c++
	      (setq base (replace-regexp-in-string
			  "\\([a-z]\\)\\([A-Z]\\)" "\\1_\\2" base t)))
	 (concat "__" (upcase base) "__"))))

(defun proj-ns-from-filename (filename)
  (let (comps)
    (if proj-use-relative-paths
	(let ((retreat 0))
	  (setq comps
		(split-string
		 (file-relative-name (file-name-directory filename)
				     proj-toplevel) "/"))
	  (setq comps (and (not (string-equal ".." (car comps)))
			   (cons proj-name comps))))
      (setq comps (list proj-name)))
    (mapcar '(lambda (proj-i)
	       (setq proj-i (concat "-" proj-i))
	       (replace-regexp-in-string
		"[^a-zA-Z0-9_]+." '(lambda (match)
				  (upcase (substring match -1))) proj-i t))
	    comps)))

(defun proj-insert-c++-class-template ()
  (when proj-c++
    (let ((start (point)) (proj-comps-ctr 0))
      (when proj-library
	(let ((comps (proj-ns-from-filename buffer-file-name)))
	  (mapc '(lambda (item) (insert "namespace " item "\n{\n")) comps)
	  (setq proj-comps-ctr (length comps))))
      (insert "class " (file-name-sans-extension
			(file-name-nondirectory buffer-file-name))
	      "\n{\nprivate:\npublic:\n};")
      (while (> proj-comps-ctr 0)
	(insert "\n}")
	(setq proj-comps-ctr (1- proj-comps-ctr)))
      (normal-mode)
      (indent-region start (point) nil))))

(provide 'proj)

;;; PROJ.EL ends here
