;;; EMPI-DIRED.EL --- Dired extensions for EMPI

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	16 Jul 2004
;; Version: 	1.0
;; Keywords:	music empi dired

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

;; These are a few functions for interaction of EMPI with dired buffers.
;; To set up the keybindings for this file, just add something like this
;; during initialisation:
;;
;;	(eval-after-load "dired"
;;	  '(progn
;;	     (require 'empi-dired)
;;	     (define-key dired-mode-map [(control ?e)] empi-dired-map)))
;;

;;; Code:

(require 'empi-core)
(require 'dired)

(defvar empi-dired-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?e] 'empi-dired-do-enqueue)
    map)
  "Keymap for EMPI operations in dired mode.")

(defun empi-dired-enqueue ()
  (let ((file (dired-get-filename)))
    (if (empi-action :enqueue file)
	(unlogged-message "Enqueued %s" file)
      (message "Unable to enqueue %s" file) nil)))

;;;###autoload
(defun empi-dired-do-enqueue (&optional arg)
  "Enqueue using EMPI the marked (or next ARG) files."
  (interactive "P")
  (let ((fail 0) (pass 0))
    (dired-map-over-marks (empi-dired-enqueue) arg)))

(provide 'empi-dired)

;;; EMPI-DIRED.EL ends here
