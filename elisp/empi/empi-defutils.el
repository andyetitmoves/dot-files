;;; EMPI-DEFUTILS.EL --- Utilities for writing EMPI definitions

;; Copyright (C) 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	20 Feb 2005
;; Version: 	1.0
;; Keywords:	empi music

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

(defalias 'ston 'string-to-number)

(defun list-all-occur (regexp str &optional foreach n)
  (or n (setq n 1))
  (or (functionp foreach) (setq foreach nil))
  (let ((lastpos 0) (case-fold-search nil))
    (while (string-match regexp str lastpos)
      (if (functionp foreach)
	  (save-match-data
	    (funcall foreach (match-string n str)))
	(setq foreach (cons (match-string n str) foreach)))
      (setq lastpos (match-end 0)))
    (if (functionp foreach) t (nreverse foreach))))

(defsubst streq (val1 val2)
  (if (string= val1 val2) 1 0))

(defsubst safe-streq (val1 val2)
  (if (and (stringp val1) (stringp val2) (string= val1 val2)) 1 0))

(defsubst numpred (val) (if val 1 0))
(defsubst prednum (val) (if (= val 0) nil t))

(defsubst toggle-str (args &optional on off toggle)
  (if (and args (car args))
      (if (> (prefix-numeric-value (car args)) 0) on off) toggle))

(defun empi-adjust-pos (ctx cmd &rest args)
  (mapcar '(lambda (item) (number-to-string (1+ item))) args))

(provide 'empi-defutils)

;;; EMPI-DEFUTILS.EL ends here
