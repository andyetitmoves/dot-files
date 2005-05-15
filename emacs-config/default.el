;;;; Paths

(eval-and-compile
  (defvar ramk-home "/home/ramk")

  (defun add-ramk-subdir (path)
    (add-to-list 'load-path (concat ramk-home "/elisp/" path)))

  (mapc 'add-ramk-subdir '("" "libmpdee" "empi" "empi/defs")))

;;;; Custom

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(apropos-do-all t)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t nil (image-file))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(backward-delete-char-untabify-method nil)
 '(blink-cursor t)
 '(browse-url-browser-function (quote browse-url-w3))
 '(c-auto-newline t t)
 '(c-basic-offset 4)
 '(c-cleanup-list (quote (brace-catch-brace empty-defun-braces defun-close-semi list-close-comma scope-operator space-before-funcall)))
 '(c-default-style (quote ((c-mode . "linux") (java-mode . "java") (other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(c-ignore-auto-fill nil)
 '(c-indent-level 4 t)
 '(c-offsets-alist (quote ((substatement-open . 0) (case-label . -))))
 '(c-tab-always-indent t)
 '(calendar-remove-frame-by-deleting t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compile-prompt "Compile [%w] ")
 '(confirm-kill-emacs nil)
 '(cperl-auto-newline t)
 '(cperl-clobber-lisp-bindings (quote null))
 '(cperl-hairy t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-lazy-help-time 2)
 '(cperl-pod-here-scan nil)
 '(cperl-under-as-char t)
 '(delete-old-versions t)
 '(desktop-base-file-name "desktop")
 '(desktop-globals-to-save nil)
 '(desktop-locals-to-save nil)
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save-mode t nil (desktop))
 '(dired-recursive-copies (quote top))
 '(ecb-download-delete-archive nil)
 '(ecb-options-version "2.26")
 '(ecb-tip-of-the-day-file "~/.emacs.d/ecb-tip-of-day")
 '(ecb-windows-width 0.25)
 '(ede-project-placeholder-cache-file "~/.emacs.d/projects.ede")
 '(eldoc-mode t t)
 '(elp-reset-after-results nil)
 '(elp-sort-by-function (quote elp-sort-by-total-time))
 '(emacs-lisp-mode-hook (quote (semantic-default-elisp-setup)))
 '(empi-default-player "mpd")
 '(empi-initial-backends (quote (empi-mpd empi-mpc empi-forwarder empi-dummy)))
 '(empi-mode-line-playtime-mode t nil (empi))
 '(empi-player-alist (quote (("mpd" (empi-mpc :restrict (:qpltitles :qplfiles)) empi-mpd empi-mpc empi-forward) ("dummy" empi-dummy))))
 '(empl-playlist-locked t)
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(global-semantic-highlight-by-attribute-mode nil nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
 '(gnus-directory "~/.emacs.d/gnus")
 '(goto-address-url-face (quote bold))
 '(grep-find-prompt "Find [%w] ")
 '(grep-highlight-matches t)
 '(grep-prompt "Grep [%w] ")
 '(hfy-optimisations (quote (merge-adjacent-tags zap-string-links keep-overlays)))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-mode (quote buffer) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-use-filename-at-point t)
 '(inhibit-startup-message t)
 '(ispell-program-name "aspell")
 '(kill-whole-line t)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-log-max 1000)
 '(mouse-wheel-mode nil nil (mwheel))
 '(next-error-highlight t)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(proj-c++ t)
 '(proj-description "Lab Information Management System")
 '(proj-library t)
 '(proj-name "Lims")
 '(proj-toplevel "~/programs/lims/src/")
 '(proj-use-relative-paths t)
 '(scroll-bar-mode nil)
 '(semantic-which-function-use-color t)
 '(semanticdb-default-save-directory "~/.emacs.d/semantic-cache")
 '(semanticdb-project-roots (quote ("~/programs")))
 '(session-locals-include (quote (buffer-read-only view-mode)))
 '(session-save-file "~/.emacs.d/session")
 '(session-undo-check -1)
 '(sh-shell-arg (quote ((bash . "-i") (csh . "-f") (pdksh) (ksh88) (rc . "-p") (wksh) (zsh . "-f"))))
 '(shell-command-completion-mode t)
 '(shell-command-on-region-prompt "CmdReg [%w] ")
 '(shell-command-prompt "Cmd [%w] ")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(speedbar-show-unknown-files t)
 '(speedbar-track-mouse-flag t)
 '(speedbar-use-tool-tips-flag nil)
 '(table-time-before-reformat 0)
 '(table-time-before-update 0)
 '(tempbuf-minimum-timeout 60)
 '(thumbs-thumbsdir "~/.emacs.d/thumbs")
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-gud-tips-p t nil (tooltip))
 '(tooltip-mode t nil (tooltip))
 '(tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(url-automatic-caching t)
 '(user-mail-address "andyetitmoves@gmail.com")
 '(version-control t)
 '(view-read-only t)
 '(view-remove-frame-by-deleting t)
 '(w3-configuration-directory "~/.emacs.d/w3/")
 '(w3-do-incremental-display t)
 '(w3-honor-stylesheets t)
 '(w3-horizontal-rule-char 45)
 '(w3-use-terminal-characters t)
 '(w3-user-colors-take-precedence t)
 '(x-select-enable-clipboard t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((((type tty)) (:background "black" :foreground "white")) (t (:background "black" :foreground "white" :slant normal :weight bold :height 101 :width normal :family "adobe-courier"))))
 '(bold ((t (:foreground "lightyellow" :underline t :weight bold :width semi-expanded))))
 '(bold-italic ((t (:foreground "grey80" :slant italic :weight bold :width semi-expanded))))
 '(border ((t (:background "grey70"))) t)
 '(cperl-array-face ((((class color) (background dark)) (:inherit font-lock-variable-name-face :slant italic))))
 '(cperl-hash-face ((((class color) (background dark)) (:inherit font-lock-variable-name-face :underline t))))
 '(cursor ((t (:background "lightgreen"))) t)
 '(fixed-pitch ((t (:family "fixedsys"))))
 '(fringe ((((class color) (background dark)) (:background "grey20"))))
 '(header-line ((((class color grayscale) (background dark)) (:inherit mode-line :background "grey30" :box (:line-width 4 :color "cyan" :style released-button) :width semi-expanded))))
 '(highlight ((((class color) (background dark)) (:background "cyan" :foreground "darkblue"))))
 '(highlight-changes-delete-face ((((class color)) (:foreground "yellow" :underline t))))
 '(highlight-changes-face ((((class color)) (:foreground "lightyellow"))))
 '(info-node ((((class color) (background dark)) (:foreground "cyan" :weight bold))))
 '(info-xref ((t (:foreground "yellow" :underline t :weight bold))))
 '(menu ((((type x-toolkit)) (:background "black" :foreground "cyan"))))
 '(mode-line ((((type x w32 mac) (class color)) (:foreground "cyan" :box (:line-width -1 :style released-button)))))
 '(mouse ((t (:background "white" :foreground "blue"))) t)
 '(next-error ((t (:background "yellow" :foreground "red"))))
 '(region ((t (:background "cyan" :foreground "darkblue"))))
 '(rfcview-headname-face ((t (:inherit info-node))))
 '(rfcview-headnum-face ((t (:inherit info-node))))
 '(rfcview-mouseover-face ((t (:inherit highlight))))
 '(rfcview-rfcnum-face ((t (:foreground "orange"))))
 '(rfcview-stdnum-face ((t (:foreground "lightsteelblue"))))
 '(rfcview-title-face ((t (:inherit (Info-title-2-face bold)))))
 '(scroll-bar ((t (:background "grey30" :foreground "lightyellow"))) t)
 '(semantic-tag-boundary-face ((((class color) (background dark)) (:weight semi-bold :width semi-expanded))))
 '(show-paren-match-face ((((class color)) (:background "grey28"))))
 '(show-paren-mismatch-face ((((class color)) (:background "lightpink" :foreground "white"))))
 '(table-cell-face ((t (:background "grey10" :foreground "cyan" :inverse-video nil))))
 '(tooltip ((((class color)) (:background "black" :foreground "green" :weight bold))))
 '(trailing-whitespace ((t (:background "grey20"))))
 '(underline ((t (:foreground "lightblue" :underline t)))))

(custom-theme-set-faces
 'user
 '(default
    ((((type tty)) (:background "black" :foreground "white"))
     (t (:background "black" :foreground "white" :slant normal :weight bold
		     :height 101 :width normal :family "adobe-courier")))))

;;;; Disabled

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;; Autoloads

(and (boundp 'generated-autoload-file)
     (or (file-writable-p generated-autoload-file)
	 (setq generated-autoload-file "~/.emacs.d/loaddefs.el")))

(and (file-exists-p "~/.emacs.d/loaddefs.el")
     (let ((load-path (cons "~/.emacs.d" load-path)))
       (load "loaddefs")))

;;;; Binding Helpers

(defun hi-backspace ()
  (interactive)
  (if (equal last-command 'hippie-expand)
      (hippie-expand -1)
    (backward-delete-char-untabify 1)))

(defun smart-home ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (let ((bpos (line-beginning-position)) nwpos)
      (or (setq nwpos (string-match "[^ \t]" (buffer-substring bpos (point))))
	  (setq nwpos 0))
      (goto-char (+ bpos nwpos)))))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun read-library-find-library (lib)
  (interactive (list (read-library "Find Library: ")))
  (find-library lib))

(or (fboundp 'define-keys)
    (defun define-keys (map &rest args)
      (while (cadr args)
	(define-key map (car args) (cadr args))
	(setq args (cddr args)))))

;;;; Bindings

(pc-bindings-mode)

(define-keys (current-global-map)
  [remap backward-delete-char-untabify]	'hi-backspace
  [remap beginning-of-line]		'smart-home
  [remap find-library]	                'read-library-find-library
  [?\t]					'hippie-expand
  [C-f3]				'find-grep-dired
  [f4]					'kill-this-buffer
  [(control ?x) ?k]			'kill-this-buffer
  [f7]					'compile
  [(control ?`)]			'indent-according-to-mode
  [insert]				'back-to-indentation
  [M-insert]				'overwrite-mode
  [(control ?n)]			'open-line
  [(control ?o)]			'other-window
  [C-escape]				'switch-to-other-buffer
  [(control ?a)]			'mark-whole-buffer
  [?\(]					'insert-parentheses
  [(meta ?f)]				'find-function
  [(meta ?v)]				'find-variable
  [(meta ?l)]				'find-library
  [(control ?\?)]			'redo
  [(control ?,)]			'pop-global-mark
  [mouse-3]				'mouse-popup-menubar-stuff
  [(control mouse-3)]			'mouse-save-then-kill
  [(control return)]			'source-jump-at-point
  [M-return]				'show-doc-at-point
  )

(eval-after-load "empi" '(global-set-key [(control ?e)] empi-map))

(require 'ffap)
(eval (cons 'progn (cons '(global-set-key [M-S-mouse-3] 'ffap-at-mouse)
			 (cdr ffap-bindings))))

(define-keys emacs-lisp-mode-map
  [?\t]			'hippie-expand
  [return]		'newline-and-indent
  [f8]			'emacs-lisp-byte-compile
  [(control f8)]	'emacs-lisp-byte-compile-and-load
  [f9]			'edebug-defun
  [return]		'newline-and-indent
  )

(defvar view-mode-map)
(defun view-after-load-hook ()
  ;; [return] causes incorrect overriding with help-mode
  (define-key view-mode-map [return] 'source-jump-at-point)
  (define-key view-mode-map [mouse-2] 'source-jump-at-mouse)
  ;; Our view-mode clobbers mouse-2 as well.
  (eval-after-load "help"
    '(progn
       (define-key help-xref-override-view-map [mouse-2] 'help-follow-mouse)
       (define-key help-xref-override-view-map [return] nil)))
  (define-key view-mode-map [backspace] 'pop-global-mark))

(eval-after-load "view" '(view-after-load-hook))

;;; isearch quits on backspace, if not for the code below
;;; due to a global key binding to hi-backspace above.
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

(defvar dired-file-visit-hook nil)
(defvar dired-from-buffer)

(eval-when-compile (require 'dired))

(defun dired-hooked-view-file ()
  (interactive)
  (let ((dired-from-buffer (current-buffer)))
    (dired-view-file)
    (run-hooks 'dired-file-visit-hook)))

(defun dired-hooked-find-file ()
  (interactive)
  (let ((dired-from-buffer (current-buffer)))
    (dired-find-file)
    (run-hooks 'dired-file-visit-hook)))

(defvar return-to-buffer)

(defun dired-kill-current-buffer ()
  (interactive)
  (let ((return (and (boundp 'return-to-buffer) return-to-buffer)))
    (kill-this-buffer)
    (and return (buffer-live-p return) (switch-to-buffer return))))

(defun dired-setup-kill-for-return ()
  (unless (eq major-mode 'dired-mode)
    (and (boundp 'dired-from-buffer)
	 (set (make-local-variable 'return-to-buffer) dired-from-buffer))
    (local-set-key [f3] 'dired-kill-current-buffer)
    (local-set-key [f4] 'dired-kill-current-buffer)))

(defvar dired-mode-map)
(defvar empi-dired-map)
(defun dired-after-load-hook ()
  (define-key dired-mode-map [f3] 'dired-hooked-view-file)
  (define-key dired-mode-map [f4] 'dired-hooked-find-file)
  (add-hook 'dired-file-visit-hook 'dired-setup-kill-for-return)
  (require 'empi-dired)
  (define-key dired-mode-map [(control ?e)] empi-dired-map))

(eval-after-load "dired" '(dired-after-load-hook))

(defvar tar-mode-map)
(defun tar-mode-after-load-hook ()
  (define-key tar-mode-map [return] 'tar-view)
  (define-key tar-mode-map [f3] 'tar-view)
  (define-key tar-mode-map [f4] 'tar-extract))

(eval-after-load "tar-mode" '(tar-mode-after-load-hook))

(defvar cperl-mode-map)
(defun cperl-mode-after-load-hook ()
  (define-key cperl-mode-map [f1] 'cperl-get-help))

(eval-after-load "cperl-mode" '(cperl-mode-after-load-hook))

(defvar xray-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      [?k] 'xray-click/key
      [?s] 'xray-symbol
      [?p] 'xray-position
      [?b] 'xray-buffer
      [?w] 'xray-window
      [?r] 'xray-frame
      [?m] 'xray-marker
      [?o] 'xray-overlay
      [?c] 'xray-screen
      [?f] 'xray-faces
      [?h] 'xray-hooks
      [?e] 'xray-features) map))
(global-set-key [(control ?h) ?x] xray-map)

;;;; Mode alists

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.elc$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.svn-base$" nil t))

(defun executable-file-p (filename)
  (let ((ext (file-name-extension filename)))
    (or (eq ext "exe")
	(and (eq ext nil) (not (file-directory-p filename))
	     (file-executable-p filename)))))

(defvar conv-param-list)
(defun shell-conv-after-load-hook ()
;;;(add-to-list 'conv-param-list (list 'executable-file-p "%s" nil nil t))
  (add-to-list
   'conv-param-list (list ".*\\.pdf" "pdftotext" (list "-layout" "-q")))
  (add-to-list 'auto-mode-alist '("\\.pdf$" . text-mode))
  (add-to-list 'conv-param-list (list ".*\\.pod" "pod2text"))
  (add-to-list 'auto-mode-alist '("\\.pod$" . text-mode))
  (add-to-list 'conv-param-list '(".*\\.[mM][pP]3" "id3info" nil ("%s")))
  (add-to-list 'auto-mode-alist '("\\.[mM][pP]3" . text-mode))
;;;(add-to-list 'conv-param-list (list ".*\\.ps" "pstotext"))
;;;(add-to-list 'auto-mode-alist '("\\.ps$" . text-mode))
  )
(eval-after-load "shell-convert" '(shell-conv-after-load-hook))

(fset 'perl-mode 'cperl-mode)

;;;; File / Mode hooks

(add-hook 'find-file-hooks 'shell-conv-find-file-hook)

;;;(add-hook 'find-file-hooks 'autovc-find-file-hook)

(eval-when-compile (require 'cc-mode))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-toggle-auto-hungry-state 1)))
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 8)))

(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (if (and buffer-file-name
	     (string= "elc" (downcase (file-name-extension buffer-file-name))))
	(toggle-read-only 1)
      (auto-fill-mode))))

(defun disable-trw ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook 'disable-trw)
(add-hook 'custom-mode-hook 'disable-trw)

;; The option to scan for pods at startup doesn't seem to work
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (cperl-find-pods-heres)
	     (font-lock-fontify-buffer)))

;;;; Editing

;; (defun hi-expand (arg)
;;   (interactive "P")
;;   (let ((oldpt (if (and (marker-buffer he-string-beg)
;;  			(or (= he-num -1) (equal this-command last-command)))
;;  		   (+ he-string-beg (length he-search-string))
;;  		 (point))) newpt)
;;     (call-interactively 'hippie-expand)
;;     (setq newpt (point))
;;     (and (eolp) (insert " "))
;;     (goto-char oldpt)
;;     (set-mark (point))
;;     (forward-char (- newpt oldpt))
;;     (setq deactivate-mark nil)))

(require 'redo)

;;;; Buffers

(defun filter-list (func list)
  (let (ret (item list))
    (while item
      (and (funcall func (car item))
	   (setq ret (cons (car item) ret)))
      (setq item (cdr item))) ret))

(defvar selective-buflist-exclusion-regexp
  (eval-when-compile
    (require 'make-regexp)
    (make-regexp '("\\*Messages?\\*"))))

(add-hook 'ido-make-buffer-list-hook
	  '(lambda ()
	     (setq ido-temp-list
		   (filter-list
		    '(lambda (item)
		       (not (string-match selective-buflist-exclusion-regexp
					  item))) ido-temp-list))))

(defun user-buffer-list ()
  (let (buflist)
    (mapcar '(lambda (item)
	       (or (string= (substring (buffer-name item) 0 1) " ")
		   (setq buflist (cons item buflist))))
	    (buffer-list)) buflist))

(global-set-key [(control tab)] 'ibs-select)
(defvar ibs-cycle-buffer-function)
(setq ibs-cycle-buffer-function 'user-buffer-list)

(defvar tempbuf-exclusion-regexp
  (eval-when-compile
    (require 'make-regexp)
    (make-regexp '("\\*Messages?\\*" "\\*scratch\\*" "\\*Echo Area"
		   "\\*Minibuf-" "\\*info tag table\\*" "\\*empl-buffer\\*"))))

(defvar tempbuf-inclusion-regexp "^\\*.*\\*$")

(require 'tempbuf)
(defun make-all-temp-bufs ()
  (let ((noecho (and (window-minibuffer-p)
		     (minibuffer-window-active-p (selected-window)))))
    (mapc '(lambda (item)
	     (with-current-buffer item
	       (or (not (string-match tempbuf-inclusion-regexp
				      (buffer-name)))
		   (string-match tempbuf-exclusion-regexp (buffer-name))
		   (and (boundp 'tempbuf-mode) (eq tempbuf-mode t))
		   (progn
		     (or noecho
			 (message "Adding buffer \"%s\" as a temporary buffer"
				  (buffer-name)))
		     (turn-on-tempbuf-mode)))))
	  (buffer-list))))

(run-with-idle-timer 2 t 'make-all-temp-bufs)

(fset 'orig-quit-window (symbol-function 'quit-window))
(defun quit-window (&optional nokill window)
  (interactive "P")
  (orig-quit-window (not nokill) window))

(defvar protected-buffer-variables '(view-mode buffer-read-only))

(defun save-protected-buffer-variables ()
  (mapc '(lambda (var)
	   (and (boundp var)
		(set (make-local-variable
		      (intern (concat "buffer-original-" (symbol-name var))))
		     (symbol-value var)))) protected-buffer-variables))

(defun session-locals-predicate (var buf)
  (let ((orig (intern-soft (concat "buffer-original-" (symbol-name var)))))
    (and (local-variable-p var)
	 (or (not orig)
	     (not (boundp orig))
	     (not (equal (symbol-value orig) (symbol-value var)))))))

(setq session-locals-predicate 'session-locals-predicate)

;;;; Files

(eval-after-load "auto-insert-tkld"
  '(progn
     (setq auto-insert-automatically 'ask)
     (require 'proj)
     (delete '("[]>:/]\\..*emacs" . "Emacs Lisp") auto-insert-alist)))

(setq auto-insert-exclude
      (eval-when-compile
	(require 'make-regexp)
	(make-regexp '("desktop"))))

(defvar backup-exclude-dirs
  (list temporary-file-directory small-temporary-file-directory))

(and (stringp semanticdb-default-save-directory)
     (setq backup-exclude-dirs
	   (cons (expand-file-name semanticdb-default-save-directory)
		 backup-exclude-dirs)))

(defun backup-name-in-excluded-dir-p (name)
  (let ((dir backup-exclude-dirs))
    (catch 'excluded-dir-found
      (while dir
	(and (stringp (car dir))
	     (let ((comp (compare-strings (car dir) 0 nil name 0 nil)))
	       (and (not (eq comp t))
		    (< comp (- (length (car dir))))
		    (throw 'excluded-dir-found nil))))
	(setq dir (cdr dir))) t)))
(setq backup-enable-predicate 'backup-name-in-excluded-dir-p)

;;;; Specific package init

;; Silence the compiler
(defvar dired-deletion-confirmer)
(setq dired-deletion-confirmer 'y-or-n-p)

(and window-system (setq tooltip-gud-tips-p t))

(setq compilation-environment
      '("CPPFLAGS=" "CFLAGS=-march=pentium4 -mfpmath=sse"
	"CXXFLAGS=-march=pentium4 -mfpmath=sse" "LDFLAGS="))

(defun doxymacs-add-project (name)
  (let ((home (expand-file-name "~")))
    (add-to-list 'doxymacs-doxygen-dirs
		 (list (concat "^" home "/programs/" name "/")
		       (concat home "/.doxygen.tags/" name ".xml")
		       (concat "file://" home "/programs/" name "/doc/")))))

(setq mf--source-file-extension "cc")

(setq mpg123-startup-volume nil)

(eval-after-load "rfcview" '(require 'info))

;; For with-unlogged-message
(eval-when-compile (require 'empi-core))

(defun display-fortune ()
  (interactive)
  (with-temp-buffer
    (call-process "/usr/local/games/fortune" nil t nil)
    (with-unlogged-message
     (display-message-or-buffer (buffer-string)))))

(run-with-idle-timer 60 t 'display-fortune)

;;;; After init

(add-hook 'after-init-hook
	  '(lambda ()
	     (add-hook 'find-file-hook 'save-protected-buffer-variables t)
	     (session-initialize)
	     (setq safe-load-compile-end-prompt nil)))
