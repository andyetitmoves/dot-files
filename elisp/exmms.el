;;; EXMMS.EL --- Emacs support for XMMS

;; Copyright (C) 2005 R.Ramkumar

;; Author: 	R.Ramkumar andyetitmoves@gmail.com
;; Created: 	06 Feb 2005
;; Version: 	1.0
;; Keywords:	music xmms

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
;; exmms|R.Ramkumar|andyetitmoves@gmail.com
;; |Emacs support for XMMS
;; |$Date$|$Revision$|~/packages/exmms.el

;;; Code:

(defun exmms-enqueue (fil)
  (interactive "FEnqueue what: ")
  (start-process "exmms" nil "xmms" "-e" fil))

(defun dired-exmms-enqueue ()
  (interactive)
  (exmms-enqueue (dired-get-filename)))

(defsubst echo-cmd (cmd)
  (with-temp-buffer
    (shell-command cmd t)
    (message "%s" (buffer-string))))

(defun exmms-xmms-version ()
  (interactive)
  (echo-cmd "xmms -v"))

(defun special-read-file-function (func &optional stream)
  (throw 'extract-over (funcall func stream)))

(defun special-read-file (filename func &optional init finish)
  (let ((load-read-function '(lambda(&optional stream)
			       (special-read-file-function func stream)))
	(load-source-file-function nil)
	(load-path '("."))
	(load-history nil) ret)
    (while (not ret)
      (and init (funcall init))
      (setq ret (catch 'extract-over
		  (condition-case sig
		      (load filename nil t t)
		    (error (not (eq (car sig) 'file-error))))))
      (and finish (funcall finish)))
    ret))

(defun stream-read-integer (&optional stream)
  (let (char (num-read 0))
    (while (progn (setq char (get-file-char))
		  (and (>= char 48) (<= char 57)))
      (setq num-read (+ (* num-read 10) (- char 48))))
    num-read))

(defun stream-read-line (&optional stream)
  (let (char (str ""))
    (while
	(progn
	  (setq char (get-file-char))
	  (not (or (< char 0) (= char ?\n))))
      (setq str (concat str (list char))))
    str))

(defvar exmms-inpipe "~/.xmms/inpipe" "*")
(defvar exmms-outpipe "~/.xmms/outpipe" "*")
(defvar exmms-query-command "report" "*")
(defvar exmms-output-keyword "out" "*")
(defvar exmms-output-on-cmd "on" "*")
(defvar exmms-output-off-cmd "off" "*")
(defvar exmms-output-flush-cmd "flush" "*")

(defun exmms-send-input (str)
  (interactive "sEnter string to be sent: ")
  (if (file-exists-p exmms-inpipe)
      (write-region str nil exmms-inpipe nil 1)
    (error "File %s does not exist, refusing to create" exmms-inpipe)))

(defun exmms-read-init (qstr)
  (exmms-send-input (concat exmms-output-keyword " " exmms-output-on-cmd))
  (exmms-send-input (concat exmms-query-command " " qstr)))

(defun exmms-read-finish ()
  (exmms-send-input (concat exmms-output-keyword " " exmms-output-flush-cmd))
  (exmms-send-input (concat exmms-output-keyword " " exmms-output-off-cmd)))

(defsubst exmms-query (func qstr)
  (special-read-file exmms-outpipe func '(lambda() (exmms-read-init qstr))
		     'exmms-read-finish))

(defvar exmms-left-volume -1)
(defvar exmms-right-volume -1)
(defvar exmms-vol-step 1 "*")
(defvar exmms-vol-add-cmd "add_volume %d" "*")
(defvar exmms-balance-cmd "balance %d" "*")
(defvar exmms-volume-query "volume" "*")
(defvar exmms-vol-scale-fill-char ?> "*")

(defun exmms-volume-read-function (&optional stream)
  (setq exmms-left-volume (stream-read-integer stream))
  (setq exmms-right-volume (stream-read-integer stream)))

(defun exmms-volume ()
  (interactive)
  (let ((msg t))
    (while msg
      (exmms-query 'exmms-volume-read-function exmms-volume-query)
      (setq msg (format (concat "L %d%% " (make-string exmms-left-volume
						       exmms-vol-scale-fill-char)
				"\nR %d%% " (make-string exmms-right-volume
							 exmms-vol-scale-fill-char))
			exmms-left-volume exmms-right-volume))
      (case (read-event msg)
	('up (exmms-send-input (format exmms-vol-add-cmd exmms-vol-step)))
	('down (exmms-send-input (format exmms-vol-add-cmd (- exmms-vol-step))))
	('left (exmms-send-input (format exmms-balance-cmd
					 (- (- exmms-right-volume exmms-left-volume)
					    exmms-vol-step))))
	('right (exmms-send-input (format exmms-balance-cmd
					  (+ (- exmms-right-volume exmms-left-volume)
					     exmms-vol-step))))
	('f5)
	(?q (setq msg nil))
	(t (ding))))))

(defun exmms-jump-to-time (secs)
  (interactive "sEnter time to jump to: ")
  (cond
   ((numberp secs))
   ((stringp secs)
    (let (num (parts (split-string secs ":")))
      (or parts (error "Invalid time syntax"))
      (and parts (setq num (string-to-number (pop parts))))
      (and parts (setq num (+ (* num 60) (string-to-number (pop parts)))))
      (setq num (* num 1000))
      (and parts (setq num (+ num (string-to-number (pop parts)))))
      (and parts (error "Invalid time syntax"))
      (setq secs num)))
   (t (error "Argument must be a number or a string")))
  (exmms-send-input (format "jump_to_time %d" secs)))

(defun exmms-jump-to-item (item)
  (interactive "sEnter the number of name to where you want to jump: ")
  (cond
   ((numberp item) (exmms-send-input (format "playlist goto %d" item)))
   ((stringp item)
    (let (itno)
      (setq itno (string-to-number item))
      (if (and itno (> itno 0) (integerp itno))
	  (exmms-send-input (format "playlist goto %d" itno))
	(exmms-send-input (format "playlist jump %s" item)))))
   (t (error "Argument must be a number or a string"))))

(defvar exmms-time-query "output_time" "*")

(defun exmms-time-string ()
  (let ((otime (/ (exmms-query 'stream-read-integer exmms-time-query) 1000)))
    (format "%d:%02d" (/ otime 60) (mod otime 60))))

(defvar exmms-paused-query "is_paused" "*")
(defvar exmms-playing-query "is_playing" "*")

(defun exmms-play-state ()
  (if (= (exmms-query 'stream-read-integer exmms-playing-query) 0) 0
    (if (= (exmms-query 'stream-read-integer exmms-paused-query) 0) 1 2)))

(defvar exmms-scroll-caption t "*")
(defvar exmms-scroll-caption-threshold 89 "*")
(defvar exmms-scroll-caption-step 1 "*")
(defvar exmms-scroll-caption-pos 0)
(defvar exmms-title-end-indicator " *** " "*")

(defun exmms-make-caption (time state title)
  (let (caption (thres 0))
    (setq caption
	  (concat "[" time "] "
		  (unless (= state 1)
		    (concat "< " (if (= state 2) "Paused" "Stopped") " > "))))
    (unless (and (stringp exmms-scroll-caption)
		 (string= exmms-scroll-caption "a"))
      (setq thres (length caption)))
    (setq caption
	  (concat caption title))
    (when exmms-scroll-caption
      (if (< (length caption) exmms-scroll-caption-threshold)
	  (setq exmms-scroll-caption-pos 0)
	(setq exmms-scroll-caption-pos
	      (mod (+ exmms-scroll-caption-pos exmms-scroll-caption-step)
		   (- (length caption) thres)))
	(setq caption
	      (concat (if (> thres 0) (substring caption 0 thres))
		      (substring caption (+ exmms-scroll-caption-pos thres))
		      exmms-title-end-indicator
		      (when (> exmms-scroll-caption-pos 0)
			(substring caption thres exmms-scroll-caption-pos))))))
    caption))

(defvar exmms-title-query "title" "*")
(defvar exmms-default-frame-name nil "*")
(defvar exmms-last-title nil)
(defvar exmms-last-time-str nil)
(defvar exmms-last-play-state nil)

(defun exmms-redisplay-caption ()
  (interactive)
  (set-frame-name
   (if (and exmms-last-title exmms-last-time-str exmms-last-play-state)
       (exmms-make-caption exmms-last-time-str exmms-last-play-state exmms-last-title)
     exmms-default-frame-name)))

(defun exmms-update-caption ()
  (interactive)
  (condition-case sig
      (setq exmms-last-time-str (exmms-time-string)
	    exmms-last-play-state (exmms-play-state)
	    exmms-last-title (exmms-query 'stream-read-line exmms-title-query))
    (error (when (eq (car sig) 'file-error)
	     (setq exmms-last-time-str nil)))))

(defvar exmms-update-caption-timer nil)
(defvar exmms-caption-redisplay-timer nil)
(defvar exmms-update-caption-interval 1 "*")
(defvar exmms-caption-redisplay-interval 0.3 "*")
(defvar exmms-auto-caption-lazy nil "*")

(defun exmms-auto-caption-off ()
  (interactive)
  (when exmms-update-caption-timer
    (set-frame-name exmms-default-frame-name)
    (cancel-timer exmms-update-caption-timer)
    (cancel-timer exmms-caption-redisplay-timer)
    (setq exmms-last-time-str nil)
    (setq exmms-update-caption-timer nil)))

(defun make-func-timer (inter func lazy)
  (funcall (if lazy 'run-with-idle-timer 'run-at-time) inter inter func))

(defun exmms-auto-caption-on ()
  (interactive)
  (setq exmms-default-frame-name (frame-parameter nil 'name))
  (exmms-auto-caption-off)
  (setq exmms-update-caption-timer
	(make-func-timer exmms-update-caption-interval 'exmms-update-caption
			 exmms-auto-caption-lazy))
  (setq exmms-caption-redisplay-timer
	(make-func-timer exmms-caption-redisplay-interval 'exmms-redisplay-caption
			 exmms-auto-caption-lazy)))

(defun exmms-auto-caption-toggle ()
  (interactive)
  (if exmms-update-caption-timer (exmms-auto-caption-off) (exmms-auto-caption-on)))

(provide 'exmms)

;;; EXMMS.EL ends here
