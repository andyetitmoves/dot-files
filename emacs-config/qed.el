(setq user-init-file "~/.emacs.d/config/qed.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(case-fold-search t)
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(delete-old-versions t)
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)) t)
 '(kill-whole-line t)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-mail-address "<andyetitmoves@yahoo.co.in>")
 '(version-control t))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(pc-bindings-mode)

(global-set-key "\t" 'hippie-expand)
(global-set-key [(control ?`)] 'indent-according-to-mode)
(global-set-key [(control ?x) ?k] 'kill-current-buffer)
