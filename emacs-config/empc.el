;;;; Paths

(eval-and-compile
  (add-to-list 'load-path "/home/ramk/elisp")
  (add-to-list 'load-path "/home/ramk/elisp/empi")
  (add-to-list 'load-path "/home/ramk/elisp/empi/defs"))

(setq user-init-file "~/.emacs.d/config/empc.el")

;;;; Custom

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(confirm-kill-emacs nil)
 '(empi-default-player "mpd")
 '(empi-initial-backends (quote (empi-mpd empi-mpc empi-forwarder empi-dummy)))
 '(empi-mode-line-playtime-mode t nil (empi))
 '(empi-mode-line-control-mode t nil (empi))
 '(empi-player-alist (quote (("mpd" (empi-mpc :restrict (:qpltitles :qplfiles)) empi-mpd empi-mpc empi-forward) ("dummy" empi-dummy))))
 '(empl-playlist-locked t)
 '(inhibit-startup-message t)
 '(menu-bar-mode nil)
 '(message-log-max 0)
 '(mouse-wheel-mode nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(x-select-enable-clipboard t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "mintcream" :foreground "darkgreen" :weight bold :height 100 :family "sans"))))
 '(mode-line ((t (:box (:line-width 1) :foreground "darkgreen" :background "mintcream")))))

;;;; Binding Helpers

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;; Bindings

(pc-bindings-mode)

(global-set-key [(control ?o)] 'other-window)
(global-set-key [C-f4] 'kill-current-buffer)
(global-set-key [(control ?x) ?k] 'kill-current-buffer)
(global-set-key [C-escape] 'switch-to-other-buffer)
(global-set-key [(control ?a)] 'mark-whole-buffer)
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff)
(global-set-key [(control mouse-3)] 'mouse-save-then-kill)

(eval-after-load "empi" '(global-set-key [(control ?e)] empi-map))

(defvar dired-mode-map)
(defvar empi-dired-map)
(defun dired-after-load-hook ()
  (require 'empi-dired)
  (define-key dired-mode-map [(control ?e)] empi-dired-map))

(eval-after-load "dired" '(dired-after-load-hook))

(require 'empi)
(require 'empl)

(empl)
