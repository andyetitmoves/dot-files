;;; GUD-GLOBAL-MAP.EL --- Global key-binding map for gud.

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	16 Jul 2004
;; Version: 	1.0
;; Keywords:	gud keybindings

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

;;;; Code:

(require 'gud)

(defvar gud-global-map (make-sparse-keymap))

(defun gud-global-map-on ()
  (add-to-list 'minor-mode-map-alist (cons 'gud-global-map gud-global-map) t))

(defun gud-global-map-off ()
  (remove gud-global-map 'minor-mode-map-alist))

(defun gud-global-map-mode-enter-hook ()
  (when (boundp gud-comint-buffer)
    (gud-global-map-on)
    (with-current-buffer gud-comint-buffer
      (add-hook 'kill-buffer-hook 'gud-global-map-off nil t))))

(defun gud-use-global-map ()
  (add-hook 'gud-mode-hook 'gud-global-map-mode-enter-hook))

(provide 'gud-global-map)

;;; GUD-GLOBAL-MAP.EL ends here
