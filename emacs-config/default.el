;;;; Paths

(eval-and-compile
  (add-to-list 'load-path "/home/ramk/elisp")
  (add-to-list 'load-path "/home/ramk/elisp/empi")
  (add-to-list 'load-path "/home/ramk/elisp/empi/defs"))

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
 '(browse-url-browser-function (quote browse-url-gnome-moz))
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
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save-mode t nil (desktop))
 '(dired-recursive-copies (quote top))
 '(ecb-download-delete-archive nil)
 '(ecb-options-version "2.26")
 '(ecb-tip-of-the-day-file "~/.emacs.d/ecb-tip-of-day")
 '(ecb-windows-width 0.25)
 '(ede-project-placeholder-cache-file "/home/ramk/.emacs.d/projects.ede")
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
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scroll-bar-mode nil)
 '(semantic-which-function-use-color t)
 '(semanticdb-default-save-directory "~/.emacs.d/semantic-cache")
 '(semanticdb-project-roots (quote ("~/programs")))
 '(session-save-file "~/.emacs.d/session")
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
 '(default ((((type tty)) (:background "black" :foreground "white"))
	    (t (:background "black" :foreground "white" :slant normal :weight bold :height 101 :width normal :family "adobe-courier"))))
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

;;;; Disabled

(put 'narrow-to-region 'disabled nil)

;;;; Autoloads

(and (boundp 'generated-autoload-file)
     (or (file-writable-p generated-autoload-file)
	 (setq generated-autoload-file "~/.emacs.d/loaddefs.el")))

(and (file-exists-p "~/.emacs.d/loaddefs.el")
     (let ((load-path (cons "~/.emacs.d" load-path)))
       (load "loaddefs")))

;;;; Fully manual init starts here

(defvar ramk-home "/home/ramk")

;;;; Binding Helpers

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

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

(defun find-library (lib)
  (interactive "sFind Library: ")
  (let ((path (locate-library lib)))
    (if path
	(progn
	  (and (> (length path) 4) (string-equal (substring path -4) ".elc")
	       (setq path (substring path 0 -1)))
	  (find-file path))
      (error "Source for library %s not found in load-path" lib))))

(defun elisp-source-jump (&optional symbol)
  (interactive)
  (unless symbol
    (setq symbol (thing-at-point 'symbol))
    (and symbol (setq symbol (intern symbol))))
  (when symbol
    (let ((point (point))
	  (pos
	   (cond
	    ((fboundp symbol)
	     (if (subrp (symbol-function symbol))
		 (let ((find-function-C-source-directory source-directory))
		   (find-function-C-source
		    symbol (help-C-file-name
			    (symbol-function symbol) 'subr) nil))
	       (find-function-noselect symbol)))
	    ((boundp symbol) (find-definition-noselect symbol 'defvar))
	    ((facep symbol) (find-definition-noselect symbol 'defface))
	    ((featurep symbol) (cons (locate-library symbol) 1))
	    ((widgetp symbol) (widget-browse symbol)))))
      (when pos
	(push-mark point)
	(switch-to-buffer (car pos))
	(goto-char (cdr pos))
	(recenter '(4))))))

(defun elisp-show-doc (&optional sexp)
  (interactive)
  (let (fail)
    (unless sexp
      (setq sexp (thing-at-point 'sexp))
      (and sexp (setq sexp (condition-case nil (read sexp) (error nil)))))
    (when sexp
      (cond
       ((symbolp sexp)
	(cond
	 ((fboundp sexp) (describe-function sexp))
	 ((boundp sexp) (describe-variable sexp))
	 ((featurep sexp)
	  (let ((window (selected-window)) (lib (symbol-name sexp)))
	    (condition-case nil
		;; May not have a commentary section
		(finder-commentary lib)
	      (error
	       (setq lib (find-library-name lib))
	       (if lib (find-file-other-window lib) (setq fail t))))
	    (or fail (select-window window))))
	 ((facep sexp) (describe-face sexp))
	 ((charsetp sexp) (describe-character-set sexp))
	 ((coding-system-p sexp) (describe-coding-system sexp))
	 ((widgetp sexp) (widget-browse sexp))
	 ((setq fail t))))
       ((stringp sexp)
	(cond
	 ((key-binding sexp) (describe-key sexp))
	 ((fontset-name-p sexp) (describe-fontset sexp))
	 ((setq fail t))))
       ((char-or-string-p sexp)
	(describe-char sexp))
       ((vectorp sexp)
	(cond
	 ((key-binding sexp) (describe-key sexp))
	 ((setq fail t))))
       ((listp sexp)
	(cond
	 ((not (cdr sexp)) (elisp-show-doc (car sexp)))
	 ((eq (car sexp) 'quote) (elisp-show-doc (cdr sexp)))
	 ((setq fail t))))
       ((setq fail t))))
    (and sexp (not fail))))

(defun cperl-show-doc (&optional word verbose)
  (interactive)
  (or word (setq word (cperl-word-at-point)))
  (let* (found (try word) (toks (split-string try "::")))
    (while (and (not found) try)
      (and verbose (message "Trying perldoc for %s" try))
      (setq found (call-process "perldoc" nil nil nil "-l" try))
      (or (setq found (if (and (numberp found) (= found 0)) t))
	  (setq toks (nbutlast toks)
		try (and toks (mapconcat 'identity toks "::")))))
    (save-excursion
      (or (and try (cperl-perldoc try))
	  (let (cperl-message-on-help-error)
	    (car (cperl-describe-perl-symbol word)))))))

(defvar show-doc-function-alist
  '((emacs-lisp-mode . elisp-show-doc)
    (cperl-mode . cperl-show-doc)))

(defun show-doc-at-point ()
  (interactive)
  (let ((sexp (assq major-mode show-doc-function-alist)))
    (or (and sexp (funcall (cdr sexp)))
	(and (fboundp 'ffap-guesser)
	     (setq sexp (ffap-guesser))
	     (cond
	      ((ffap-url-p sexp) (browse-url sexp))
	      ((and (file-exists-p sexp) (setq sexp (find-file-noselect sexp)))
	       (display-buffer sexp t))))
	(message "No appropriate documentation found"))))

(defvar source-jump-function-alist
  '((emacs-lisp-mode . elisp-source-jump)
    (t . semantic-complete-jump-local)))

(defun source-jump ()
  (interactive)
  (let ((def (or (assq major-mode source-jump-function-alist)
		 (assq t source-jump-function-alist))))
    (and def (funcall (cdr def)))))

(defun source-jump-at-mouse (evt)
  (interactive "e")
  (let* ((start-posn (event-start evt))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn)))
    (select-window start-window)
    (goto-char start-point))
  (source-jump))

;;;; Bindings

(pc-bindings-mode)

(global-set-key "\t" 'hippie-expand)
(global-set-key [backspace] 'hi-backspace)
(global-set-key [C-f3] 'find-grep-dired)
(global-set-key [f3] 'kill-current-buffer)
(global-set-key [f4] 'kill-current-buffer)
(global-set-key [f7] 'compile)
(global-set-key [(control ?`)] 'indent-according-to-mode)
(global-set-key [insert] 'back-to-indentation)
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key [(control ?n)] 'open-line)
(global-set-key [(control ?o)] 'other-window)
(global-set-key [C-f4] 'kill-current-buffer)
(global-set-key [(control ?x) ?k] 'kill-current-buffer)
(global-set-key [C-escape] 'switch-to-other-buffer)
(global-set-key [(control ?a)] 'mark-whole-buffer)
(global-set-key [?\(] 'insert-parentheses)
(global-set-key [(meta ?f)] 'find-function)
(global-set-key [(meta ?v)] 'find-variable)
(global-set-key [(meta ?l)] 'find-library)
(global-set-key [(control ?\?)] 'redo)
(global-set-key [home] 'smart-home)
(global-set-key [(control ?,)] 'pop-global-mark)
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff)
(global-set-key [(control mouse-3)] 'mouse-save-then-kill)
(global-set-key [(control return)] 'source-jump)
(global-set-key [M-return] 'show-doc-at-point)

(eval-after-load "empi" '(global-set-key [(control ?e)] empi-map))

(require 'ffap)
(eval (cons 'progn (cons '(global-set-key [M-S-mouse-3] 'ffap-at-mouse)
			 (cdr ffap-bindings))))

(define-key emacs-lisp-mode-map [?\t] 'hippie-expand)
(define-key emacs-lisp-mode-map [return] 'newline-and-indent)
(define-key emacs-lisp-mode-map [f8] 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map
  [(control f8)] 'emacs-lisp-byte-compile-and-load)
(define-key emacs-lisp-mode-map [f9] 'edebug-defun)
(define-key emacs-lisp-mode-map [f10] 'edebug-next-mode)
(define-key emacs-lisp-mode-map [return] 'newline-and-indent)

(defvar view-mode-map)
(defun view-after-load-hook ()
  ;; [return] causes incorrect overriding with help-mode
  (define-key view-mode-map [return] 'source-jump)
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

(defvar dired-mode-map)
(defvar empi-dired-map)
(defun dired-after-load-hook ()
  (require 'empi-dired)
  (define-key dired-mode-map [(control ?e)] empi-dired-map)
  (define-key dired-mode-map [f3] 'dired-view-file)
  (define-key dired-mode-map [f4] 'dired-find-file))

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
    (define-key map [?k] 'xray-click/key)
    (define-key map [?s] 'xray-symbol)
    (define-key map [?p] 'xray-position)
    (define-key map [?b] 'xray-buffer)
    (define-key map [?w] 'xray-window)
    (define-key map [?r] 'xray-frame)
    (define-key map [?m] 'xray-marker)
    (define-key map [?o] 'xray-overlay)
    (define-key map [?c] 'xray-screen)
    (define-key map [?f] 'xray-faces)
    (define-key map [?h] 'xray-hooks)
    (define-key map [?e] 'xray-features) map))
(global-set-key [(control ?h) ?x] xray-map)

;;;; Mode alists

(add-to-list 'auto-mode-alist '(".*\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '(".*\\.tcc$" . c++-mode))
(add-to-list 'auto-mode-alist '(".*\\.elc$" . emacs-lisp-mode))

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
  (add-to-list 'auto-mode-alist '(".*\\.pdf$" . text-mode))
  (add-to-list 'conv-param-list (list ".*\\.pod" "pod2text"))
  (add-to-list 'auto-mode-alist '(".*\\.pod$" . text-mode))
  (add-to-list 'conv-param-list '(".*\\.[mM][pP]3" "id3info" nil ("%s")))
  (add-to-list 'auto-mode-alist '(".*\\.[mM][pP]3" . text-mode))
;;;(add-to-list 'conv-param-list (list ".*\\.ps" "/home/ramk/pstotext"))
;;;(add-to-list 'auto-mode-alist '(".*\\.ps$" . text-mode))
  )
(eval-after-load "shell-convert" '(shell-conv-after-load-hook))

(fset 'perl-mode 'cperl-mode)

;;;; File / Mode hooks

(add-hook 'find-file-hooks 'shell-conv-find-file-hook)

;;;(add-hook 'find-file-hooks 'autovc-find-file-hook)

;; Silence the compiler
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

(defun disable-trw () (setq show-trailing-whitespace nil))

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
  (list (expand-file-name semanticdb-default-save-directory)
	temporary-file-directory small-temporary-file-directory))
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
  (add-to-list 'doxymacs-doxygen-dirs
	       (list (concat "^/home/ramk/programs/" name "/")
		     (concat "/home/ramk/.doxygen.tags/"
			     name ".xml")
		     (concat "file:///home/ramk/programs/" name "/doc/"))))

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
	     (session-initialize)
	     (setq safe-load-compile-end-prompt nil)))
