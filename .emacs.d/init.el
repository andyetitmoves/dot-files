;; Emacs Init File: Just a wrapper around the actual config file

(defvar ramk-home (getenv "HOME"))
(add-to-list 'load-path (concat ramk-home "/elisp/misc"))
(require 'safe-load)
(let ((profile (getenv "EMACS_PROFILE")) load-arg)
  (or (and profile
	   (load (setq load-arg (concat "~/.emacs.d/config/" profile)) t))
      (load (setq load-arg "~/.emacs.d/config/default") t))
  (setq user-init-file (concat load-arg ".el")))
