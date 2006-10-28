;;; MPDEE-AUX.EL --- Utilities for the libmpdee package

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	16 May 2004
;; Version: 	1.0
;; Keywords:	mpd, music

;; This file is *NOT* part of GNU Emacs

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

(require 'libmpdee)
(require 'flexi-print)

(defun mpd-jump-to-time (conn time)
  (interactive
   (list mpd-inter-conn
	 (mpd-read-item "Time in seconds"
			(and (eq (mpd-connection-status mpd-inter-conn) 'ready)
			     (plist-get (mpd-get-status mpd-inter-conn)
					'time-elapsed)) t)))
  (let ((status (mpd-get-status conn)))
    (and status (and (mpd-seek conn (plist-get status 'song) time) status))))

(defun mpd-skip-time (conn time)
  (interactive (list mpd-inter-conn (mpd-read-item "Seconds to skip by" 0 t t)))
  (mpd-assert-numberp time)
  (let ((status (mpd-get-status conn)))
    (and status
	 (mpd-seek conn (plist-get status 'song)
		   (if (< (setq time
				(+ (plist-get status 'time-elapsed) time)) 0)
		       0 time)) status)))

(defsubst mpd-jump-to-time-msec (conn time)
  (mpd-jump-to-time conn (floor (/ time 1000))))

(defsubst mpd-decrease-volume (conn by)
  (mpd-adjust-volume conn (- by)))

(defsubst mpd-get-volume (conn)
  (plist-get (mpd-get-status conn) 'volume))

(defun mpd-compat-play (conn)
  (interactive (list mpd-inter-conn))
  (let ((status (and (eq (mpd-connection-status conn) 'ready)
		     (mpd-get-status conn))))
    (and (mpd-play conn (and status (plist-get status 'song))) status)))

(defun mpd-compat-stop (conn)
  (interactive (list mpd-inter-conn))
  (let ((status (and (eq (mpd-connection-status conn) 'ready)
		     (mpd-get-status conn))) state (ret t))
    (setq state (if status (plist-get status 'state) 'stop))
    (if (eq state 'play) (setq ret (mpd-pause conn)))
    (or (eq state 'stop) (setq ret (mpd-seek conn (plist-get status 'song))))
    (and ret status)))

(defsubst mpd-convert-compat-pausedp (status)
  (and (eq (plist-get status 'state) 'pause)
       (not (= (plist-get status 'time-elapsed) 0))))

(defun mpd-compat-pausedp (conn)
  (interactive (list mpd-inter-conn))
  (mpd-convert-compat-pausedp (mpd-get-status conn)))

(defun mpd-time-ms-format (time)
  (if (not (numberp time))
      nil
    (format "%d:%02d" (/ time 60) (mod time 60))))

(defun mpd-song-data-query (data query)
  (let ((value (plist-get data (intern query))))
    (and value (string= query "Time")
	 (setq value (mpd-time-ms-format value))) value))

(defvar mpd-song-data-flexi-scheme
  (eval-when-compile
    (flexi-print-compile
     :default '(lambda ()
		 (mpd-song-data-query flexi-print-cookie flexi-print-query)))))

(defun mpd-format-title (conn fmt &optional song)
  (let ((flexi-print-cookie song))
    (flexi-print mpd-song-data-flexi-scheme fmt)))

(defsubst mpd-pausedp (conn)
  (eq (plist-get (mpd-get-status conn) 'state) 'pause))

(defsubst mpd-playingp (conn)
  (eq (plist-get (mpd-get-status conn) 'state) 'play))

(defsubst mpd-get-elapsed-time (conn)
  (plist-get (mpd-get-status conn) 'time-elapsed))

(defsubst mpd-repeat-flag (conn)
  (plist-get (mpd-get-status conn) 'repeat))

(defsubst mpd-random-flag (conn)
  (plist-get (mpd-get-status conn) 'random))

;;;###autoload
(defun mpd-enqueue-from-log (conn)
  (interactive (list mpd-inter-conn))
  (with-mpd-free-buffer
    (forward-line 0)
    (if (looking-at "\\w+ +[0-9]+ +[0-9]+:[0-9]+ : \
\\(?:added\\|updating\\) \\(.*\\)")
	(let ((file (match-string 1)))
	  (if (mpd-enqueue conn file)
	      (message "Enqueued file %s" file)
	    (error "Unable to enqueue file %s: %s" file
		   (mpd-get-last-error conn))))
      (error "No file on current line"))))

(provide 'mpdee-aux)

;;; MPDEE-AUX.EL ends here
