;;; QUICKEY.EL --- Multiple key bindings differentiated by keypress speeds.

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	17 May 2004
;; Version: 	1.0
;; Keywords:	key bindings

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
;; quickey|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Multiple key bindings differentiated by keypress speeds.
;; |$Date: 2004/05/17 15:02:22 $|$Revision: 1.1 $|~/packages/quickey.el

;;; Commentary:

;; With an editor like Emacs with thousands of functions, it becomes very common
;; for keybindings to increase in length, making them only as accessible as the
;; M-x <command> counterpart. So, here is an alternative approach: Bind many
;; keybindings to one key, set a small timeout depending on your typing speed,
;; and execute one action based on the number of keystrokes of that key in the
;; timeout period. Obviously, tapping the same key maybe twice quickly in
;; succession is not that tedious as something like C-x 5 C-f. The flip side -
;; you've always have to wait for the timeout period, which you probably won't
;; notice being something like 0.2 seconds for 2 keybindings. It is suggested you
;; use this for single keystrokes like TAB or INS for two bindings (maybe three
;; if you are really quick) and keep the timeout to 0.2 or 0.3 seconds.

;;; Installation:

;; Just put this on your load-path and preferably byte-compile it. Add the
;; following to your initialisation file:
;; 	(require 'quickey)
;;	(global-quickey-bind-forms ...) / (global-quickey-bind ...)
;; You could put the `quickey-bind-forms' / `quickey-bind' calls to your mode
;; hooks as well. Refer to the documentation for these constructs for more
;; details.

;;; Code:

(defvar working-report-execution t
  "*Report the form executed in a quickey keyset to the echo area.")

(require 'working)

;;;###autoload
(defun quickey (keyset)
  "Act on quickey keyset KEYSET to execute or schedule an action specified.
This function is not meant to be used directly. Refer `quickey-bind-forms',
`quickey-bind', `global-quickey-bind-forms' and `global-quickey-bind' for using
the quickey package."
  (interactive)
  (or (vectorp keyset)
      (error "Invalid keyset, probably invoked directly. Refer documentation"))
  (let ((chain (aref keyset 1)))
    (if chain
	(and (cdr chain) (aset keyset 1 (cdr chain)))
      (aset keyset 1 (aref keyset 2))
      (run-with-timer (aref keyset 0) nil
		      '(lambda (keyset)
			 (and working-report-execution
			      (working-message "Quickey: %s"
					       (car (aref keyset 1))))
			 (unwind-protect
			     (eval (car (aref keyset 1)))
			   (aset keyset 1 nil))) keyset))))

(defsubst quickey-bind-forms (keymap key timeout &rest args)
  "Define a key-binding to a lisp form using quickey.
The keybinding is done using KEYMAP for KEY. TIMEOUT is the time before which
the number of keypresses of KEY determines which of the forms in ARGS to
execute. See `quickey-bind' for a simpler though less powerful of specifying
each of the actions."
  (define-key keymap key (list 'lambda nil '(interactive)
			       (list 'quickey (vector timeout nil args)))))

(defun quickey-bind (keymap key timeout &rest args)
  "Define a key-binding to a string/function using quickey.
The keybinding is done using KEYMAP for KEY. TIMEOUT is the time before which
the number of keypresses of KEY determines which of the ARGS to execute. Each of
the ARGS can be a string to be inserted to the current buffer, a command to call
interactively, a function to call, or a symbol referring to a command. See
`quickey-bind-forms' for a more general way of specifying the binding."
  (apply 'quickey-bind-forms keymap key timeout
	 (mapcar '(lambda (item)
		    (cond
		     ((stringp item) (list 'insert item))
		     ((commandp item) (list 'call-interactively (list 'quote item)))
		     ((functionp item) (list item))
		     ((symbolp item) (list 'call-interactively item)))) args)))

(defmacro global-quickey-bind-forms (key timeout &rest args)
  "Define a global-binding as in `quickey-bind-forms'."
  `(quickey-bind-forms global-map ,key ,timeout ,@args))

(defmacro global-quickey-bind (key timeout &rest args)
  "Define a global-binding as in `quickey-bind'."
  `(quickey-bind global-map ,key ,timeout ,@args))

(provide 'quickey)

;;; QUICKEY.EL ends here
