;;; DWIM-ATPT.EL --- 'Do what I mean' at point for doc, source etc.

;; Copyright (C) 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	19 Mar 2005
;; Version: 	1.0
;; Keywords:	doc

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

;;;###autoload
(defun elisp-show-doc (&optional sexp)
  (interactive)
  (let (fail)
    (unless sexp
      (setq sexp (thing-at-point 'sexp))
      (and sexp (setq sexp (condition-case nil (read sexp) (error nil)))))
    (when sexp
      (cond
       ((symbolp sexp)
	(cond
	 ((fboundp sexp) (describe-function sexp))
	 ((boundp sexp) (describe-variable sexp))
	 ((featurep sexp)
	  (let ((window (selected-window)) (lib (symbol-name sexp)))
	    (condition-case nil
		;; May not have a commentary section
		(finder-commentary lib)
	      (error
	       (setq lib (find-library-name lib))
	       (if lib (find-file-other-window lib) (setq fail t))))
	    (or fail (select-window window))))
	 ((facep sexp) (describe-face sexp))
	 ((charsetp sexp) (describe-character-set sexp))
	 ((coding-system-p sexp) (describe-coding-system sexp))
	 ((widgetp sexp) (widget-browse sexp))
	 ((setq fail t))))
       ((stringp sexp)
	(cond
	 ((key-binding sexp) (describe-key sexp))
	 ((fontset-name-p sexp) (describe-fontset sexp))
	 ((setq fail t))))
       ((char-or-string-p sexp)
	(describe-char sexp))
       ((vectorp sexp)
	(cond
	 ((key-binding sexp) (describe-key sexp))
	 ((setq fail t))))
       ((listp sexp)
	(cond
	 ((not (cdr sexp)) (elisp-show-doc (car sexp)))
	 ((eq (car sexp) 'quote) (elisp-show-doc (cdr sexp)))
	 ((setq fail t))))
       ((setq fail t))))
    (and sexp (not fail))))

(eval-when-compile (require 'cperl-mode))

;;;###autoload
(defun cperl-show-doc (&optional word verbose)
  (interactive)
  (or word (setq word (cperl-word-at-point)))
  (let* (found (try word) (toks (split-string try "::")))
    (while (and (not found) try)
      (and verbose (message "Trying perldoc for %s" try))
      (setq found (call-process "perldoc" nil nil nil "-l" try))
      (or (setq found (if (and (numberp found) (= found 0)) t))
	  (setq toks (nbutlast toks)
		try (and toks (mapconcat 'identity toks "::")))))
    (save-excursion
      (or (and try (cperl-perldoc try))
	  (let (cperl-message-on-help-error)
	    (car (cperl-describe-perl-symbol word)))))))

(defvar show-doc-function-alist
  '((emacs-lisp-mode . elisp-show-doc)
    (cperl-mode . cperl-show-doc)))

;;;###autoload
(defun show-doc-at-point ()
  (interactive)
  (let ((sexp (assq major-mode show-doc-function-alist)))
    (or (and sexp (funcall (cdr sexp)))
	(and (fboundp 'ffap-guesser)
	     (setq sexp (ffap-guesser))
	     (cond
	      ((ffap-url-p sexp) (browse-url sexp))
	      ((and (file-exists-p sexp) (setq sexp (find-file-noselect sexp)))
	       (display-buffer sexp t))))
	(message "No appropriate documentation found"))))

;;;###autoload
(defun elisp-source-jump (&optional symbol)
  (interactive)
  (unless symbol
    (setq symbol (thing-at-point 'symbol))
    (and symbol (setq symbol (intern symbol))))
  (when symbol
    (let ((point (point))
	  (pos
	   (cond
	    ((fboundp symbol)
	     (if (subrp (symbol-function symbol))
		 (let ((find-function-C-source-directory source-directory))
		   (find-function-C-source
		    symbol (help-C-file-name
			    (symbol-function symbol) 'subr) nil))
	       (find-function-noselect symbol)))
	    ((boundp symbol) (find-definition-noselect symbol 'defvar))
	    ((facep symbol) (find-definition-noselect symbol 'defface))
	    ((featurep symbol) (cons (locate-library symbol) 1))
	    ((widgetp symbol) (widget-browse symbol)))))
      (when pos
	(push-mark point)
	(switch-to-buffer (car pos))
	(goto-char (cdr pos))
	(recenter '(4))))))

(defvar source-jump-function-alist
  '((emacs-lisp-mode . elisp-source-jump)))

(and (fboundp 'semantic-complete-jump-local)
     (setq source-jump-function-alist
	   (cons '(t . semantic-complete-jump-local)
		 source-jump-function-alist)))

;;;###autoload
(defun source-jump-at-point ()
  (interactive)
  (let ((def (or (assq major-mode source-jump-function-alist)
		 (assq t source-jump-function-alist))))
    (and def (funcall (cdr def)))))

;;;###autoload
(defun source-jump-at-mouse (evt)
  (interactive "e")
  (let* ((start-posn (event-start evt))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn)))
    (select-window start-window)
    (goto-char start-point))
  (source-jump-at-point))

(provide 'dwim-atpt)

;;; DWIM-ATPT.EL ends here
