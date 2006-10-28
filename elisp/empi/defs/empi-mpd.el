;;; EMPI-MPD.EL --- Elisp backend to MPD for EMPI

(require 'empi-defutils)
(require 'empi-elisp)
(require 'mpdee-aux)

(defgroup empi-mpd-backend nil
  "Elisp backend to MPD for EMPI."
  :prefix "empi-mpd-" :group 'empi :group 'mpd)

(defcustom empi-mpd-connection-parameters nil
  "Parameters for the mpd connection to be used by empi-mpd backend."
  :type 'mpd-connection :group 'empi-mpd-backend)

(defvar empi-mpd 'empi-elisp-command)
(defvar empi-mpd-conn
  (apply 'mpd-conn-new `(,@(mpd-connection-tidy empi-mpd-connection-parameters) nil)))

(defcustom empi-mpd-title-flexi
  '(flexi-print-backend-format
    "%{Title}%(!{Title}%{file}%)%({Artist} | %{Artist}%)%({Album} | %{Album}%)\
%({Track} | Track %{Track}%)%({Composer} | %{Composer}%)")
  "Format to be used for titles by the elisp MPD backend for EMPI."
  :type 'flexi-print-format :group 'empi-mpd-backend)

(defcustom empi-mpd-plentry-flexi
  '(flexi-print-backend-format
    "< %({fmttime}%{fmttime}%)%(!{fmttime}-:--%) > %{Title}%(!{Title}%{file}%)\
%({Artist} - %{Artist}%)%({Album} - %{Album}%)%({Track} - Track %{Track}%)\
%({Composer} - %{Composer}%)")
  "Format to be used for playlist entries by the elisp MPD backend for EMPI."
  :type 'flexi-print-format :group 'empi-mpd-backend)

(defun empi-mpd-status-to-output-list (status)
  (let (slist)
    (setq slist
	  (list :qvolume (make-list 2 (plist-get status 'volume))
		:qrepeat (plist-get status 'repeat)
		:qshuffle (plist-get status 'random)
		:qplid (plist-get status 'playlist)
		:qpllength (plist-get status 'playlistlength)
		:qcrossfade (plist-get status 'xfade)
		:playingp (numpred (eq (plist-get status 'state) 'play))
		:pausedp (numpred (eq (plist-get status 'state) 'pause))))
    (setq slist (append slist (and (plist-get status 'song)
				   (list :qplpos (plist-get status 'song)))))
    (when (plist-get status 'time-elapsed)
      (setq status
	    (list :qbitrate (plist-get status 'bitrate)
		  :qtime (* (plist-get status 'time-elapsed) 1000)
		  :qsonglength (* (plist-get status 'time-total) 1000)
		  :qfrequency (plist-get status 'sample-rate)
		  :qbits-per-sample (plist-get status 'bits-per-sample)))
      (setq slist (nconc slist status))) slist))

(defmacro empi-mpd-with-status (cmd func &rest args)
  `(lambda (conn ,@args)
     (let ((status (empi-mpd-status-to-output-list (,func conn ,@args))))
       (and status (nconc status (list ,cmd t))))))

(defun empi-mpd-status (conn)
  (let ((status (mpd-get-status conn)))
    (if status (empi-mpd-status-to-output-list status))))

(defun empi-mpd-get-title (conn)
  (let ((status (mpd-get-status conn)) song)
    (when status
      (nconc
       (and (plist-get status 'song)
	    (setq song (car (mpd-get-playlist-entry
			       conn (plist-get status 'song))))
	    (list :qtitle (mpd-format-title empi-mpd-conn
					    empi-mpd-title-flexi song)))
       (empi-mpd-status-to-output-list status)))))

(setplist 'empi-mpd
`(:prefix ,(list empi-mpd-conn) :defhandler empi-mpd-status
  :jumpitemnum mpd-seek :voladd mpd-adjust-volume :volsub mpd-decrease-volume
  :play ,(empi-mpd-with-status :play mpd-compat-play) :pause mpd-pause
  :stop ,(empi-mpd-with-status :stop mpd-compat-stop) :repeat mpd-toggle-repeat
  :shuffle mpd-toggle-random :plback mpd-prev :plnext mpd-next
  :enqueue (lambda (conn file)
	     (mpd-enqueue conn (mpd-file-to-mpd-resource file)))
  :plclear mpd-clear-playlist
  :qplfiles (lambda (conn) (list :qplfiles (mpd-get-playlist conn)))
  :jumptime ,(empi-mpd-with-status :jumptime mpd-jump-to-time-msec time)
  :qtitle empi-mpd-get-title :qvolume nil :qrepeat nil :qshuffle nil :qplid nil
  :qpllength nil :qbitrate nil :qplpos nil :qcrossfade nil :playingp nil
  :pausedp nil :qtime nil :qsonglength nil :qfrequency nil :qbits-per-sample nil
  :qchannels nil :pldel mpd-delete :plmove mpd-move :plswap mpd-swap
  :version (lambda (conn)
	     (concat "MPD "
		     (mapconcat 'number-to-string (mpd-get-version conn) ".")))
  :qpltitles (lambda (conn foreach)
 	       (mpd-get-playlist-entry
 		conn nil
 		`(lambda (song)
 		   (funcall ,foreach
 			    (mpd-format-title empi-mpd-conn
					      empi-mpd-plentry-flexi song)))) t)
  :misc (lambda (conn str)
	  (setq str (list :misc (cdr (mpd-execute-command conn str t))))
	  (mpd-get-last-error conn)
	  str)))

(provide 'empi-mpd)

;;; EMPI-MPD.EL ends here
