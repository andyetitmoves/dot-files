;;; SAFE-LOAD.EL --- Timestamp checking for loading byte-compiled files.

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	14 Jul 2004
;; Version: 	1.0
;; Keywords:	load

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

;;; Commentary:

;; A common problem facing elisp developers (atleast me) concerns byte-compiling
;; of the files they are involved in developing. While byte-compiling does speed
;; up your packages, one invariably forgets to update the compiled file after
;; making a change in the source. Worse still, you forget that the byte-compiled
;; file is getting loaded and end up getting frustrated about a change you made
;; but never saw it getting effected. So, here is a function advising before
;; load to check if there is a more recent source file lying around and
;; byte-compile it after prompting the user.

;;; Bugs:

;; This package does not work for autoloads, loading seems to get
;; short-circuited through the C code.

;;; Installation:

;; Just load this file at initialisation time.
;; The earlier, the better, as all further loads are protected.

;;; Code:

(defvar safe-load-compile-start-prompt t
  "*Prompt user before compilation on calling `check-load-file'.")

(defvar safe-load-compile-end-prompt t
  "*Prompt user after compilation on calling `check-load-file'.")

(require 'help)				; For the `locate-library' function

(defun check-load-file (file &optional nofind)
  "Update byte-compiled FILE based on filestamp of corresponding source file.
The corresponding source is the file with the same basename ( in the same
directory ) and with suffix .el. FILE should have an extension of .elc. Unless
NOFIND is non-nil, an attempt is made to locate the file using `locate-library'."
  (and (stringp file)
       (or nofind (setq file (locate-library file)))
       (let (altfile)
	 (and (string= (file-name-extension file) "elc")
	      (setq altfile
		    (substitute-in-file-name
		     (concat (file-name-sans-extension file) ".el")))
	      (file-newer-than-file-p altfile file)
	      (if (file-writable-p file)
		  (when
		      (or (not safe-load-compile-start-prompt)
			  (y-or-n-p (format "Source %s is newer, byte-compile "
					    altfile)))
		    (setq nofind (condition-case nil
				     (byte-compile-file altfile)
				   (error nil)))
		    (and safe-load-compile-end-prompt
			 (read-event
			  (format "%s byte-compiling file %s, press any key..."
				  (if nofind "Success" "Error") altfile)))
		    nofind)
		(message "File %s needs byte-compilation, but is not writable."
			 altfile) nil)))))

(require 'advice)

(defadvice load
  (before safe-load (file &optional noerror nomessage nosuffix must-suffix))
  "Check load file if byte-compiled and old, before load.
The load-file is checked with `check-load-file' before proceeding with the load."
  (check-load-file file must-suffix))

(defadvice require
  (before safe-load (feature &optional filename noerror) activate)
  "Check if require needs load of old byte-compiled file.
The file is checked with `check-load-file' before proceeding with require."
  (and (symbolp feature)
       (or (memq feature features)
	   (check-load-file (or filename (symbol-name feature))))))

;; If this file is on the load-path, do a self test
(if (check-load-file "safe-load")
    (load "safe-load")
  ;; Check over, can go for activation
  (ad-activate 'load)
  (ad-activate 'require))

(provide 'safe-load)

;;; SAFE-LOAD.EL ends here
