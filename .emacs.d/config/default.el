
;;;; Paths

(eval-and-compile
  (defvar startup-home (or (getenv "$SHOME") "~"))

  (defsubst prefix-home-subdir-to-load-path (path)
    (let ((load-dir (concat startup-home "/" path)))
      (and (file-directory-p load-dir) (add-to-list 'load-path load-dir))))

  (mapc 'prefix-home-subdir-to-load-path
	'("elisp/misc" "elisp/empi" "elisp/empi/defs"
	  "install/devel/libmpdee" ".emacs.d/site-lisp"
	  ".emacs.d/shadow")))

(defvar query-replace-from-history nil)
(defvar query-replace-to-history nil)

;;;; Custom

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-local "./")
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-electric-escape t)
 '(TeX-insert-braces nil)
 '(TeX-newline-function (quote reindent-then-newline-and-indent))
 '(TeX-parse-self t)
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))
 '(abbrev-mode t)
 '(apropos-do-all t)
 '(apt-utils-kill-buffer-confirmation-function (quote y-or-n-p))
 '(apt-utils-show-all-versions t)
 '(auto-image-file-mode t)
 '(backup-by-copying-when-mismatch t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(backward-delete-char-untabify-method (quote hungry))
 '(bibtex-align-at-equal-sign t)
 '(bibtex-entry-format (quote (opts-or-alts required-fields numerical-fields page-dashes inherit-booktitle realign last-comma delimiters unify-case)))
 '(blink-cursor-mode t nil (frame))
 '(browse-url-browser-function (quote browse-url-firefox))
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
 '(comment-multi-line t)
 '(comment-style (quote multi-line))
 '(compile-prompt "Compile [%w] ")
 '(confirm-kill-emacs nil)
 '(cperl-auto-newline t)
 '(cperl-clobber-lisp-bindings (quote null))
 '(cperl-hairy t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 4)
 '(cperl-lazy-help-time 2)
 '(cperl-pod-here-scan nil)
 '(cperl-under-as-char t)
 '(debian-bug-display-help nil)
 '(debian-bug-download-directory "~/downloads")
 '(default-input-method "tamil-itrans")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-base-file-name "desktop-state.el")
 '(desktop-globals-to-save nil)
 '(desktop-load-locked-desktop t)
 '(desktop-locals-to-save nil)
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save-mode t)
 '(develock-auto-enable nil)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(display-time-use-mail-icon t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-mode t t)
 '(elp-reset-after-results nil)
 '(elp-sort-by-function (quote elp-sort-by-total-time))
 '(empi-caption-scroll-threshold 160)
 '(empi-default-player "mpd")
 '(empi-initial-backends (quote (empi-mpd empi-mpc empi-forwarder empi-dummy)))
 '(empi-mode-line-playtime-mode t nil (empi))
 '(empi-player-alist (quote (("mpd" (empi-mpc :restrict (:qpltitles :qplfiles)) empi-mpd empi-mpc empi-forward) ("dummy" empi-dummy))))
 '(empl-playlist-locked t)
 '(eshell-directory-name "~/.emacs.d/eshell/")
 '(fill-column 80)
 '(global-font-lock-mode t)
 '(global-semantic-decoration-mode t nil (semantic/decorate/mode))
 '(global-semantic-highlight-edits-mode t nil (semantic/util-modes))
 '(global-semantic-highlight-func-mode t nil (semantic/util-modes))
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(global-semantic-idle-summary-mode t)
 '(global-semantic-idle-tag-highlight-mode t nil (semantic/idle))
 '(global-semantic-show-unmatched-syntax-mode t nil (semantic/util-modes))
 '(global-semanticdb-minor-mode t)
 '(gnus-article-button-face (quote button))
 '(gnus-directory "~/.emacs.d/gnus")
 '(gnus-extract-address-components (quote mail-extract-address-components))
 '(gnus-fetch-old-headers t)
 '(gnus-gcc-mark-as-read t)
 '(gnus-home-directory "~/.emacs.d/")
 '(gnus-init-file "~/.emacs.d/gnus/init")
 '(gnus-message-archive-group "nnml:mail.sent")
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysign t)
 '(gnus-read-newsrc-file nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-startup-file "~/.emacs.d/gnus/newsrc")
 '(gnus-summary-thread-gathering-function (quote gnus-gather-threads-by-references))
 '(gnus-thread-sort-functions (quote (gnus-thread-sort-by-number gnus-thread-sort-by-date)))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-treat-date-local (quote head))
 '(gnus-treat-display-x-face (quote head))
 '(gnus-visible-headers (quote ("^From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:" "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:" "^[BGF]?Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Mail-Followup-To:" "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:" "^X-Sent:" "^User-Agent:" "^X-Mailer:")))
 '(goto-address-url-face (quote bold))
 '(grep-find-prompt "Find [%w] ")
 '(grep-highlight-matches t)
 '(grep-prompt "Grep [%w] ")
 '(gud-tooltip-mode t)
 '(hfy-optimisations (quote (merge-adjacent-tags zap-string-links kill-context-leak div-wrapper keep-overlays)))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(html-helper-mode-uses-visual-basic t nil (html-helper-mode))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido-last.el")
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell" t)
 '(kept-new-versions 5)
 '(kept-old-versions 1)
 '(kill-whole-line t)
 '(lookup-init-file "~/.emacs.d/lookup")
 '(mail-envelope-from (quote header))
 '(mail-extr-guess-middle-initial t)
 '(mail-extr-ignore-realname-equals-mailbox-name nil)
 '(mail-source-crash-box "~/.emacs.d/gnus/mail-crash-box")
 '(mail-specify-envelope-from t)
 '(mail-user-agent (quote gnus-user-agent))
 '(makefile-electric-keys t)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-directory "~/.mail")
 '(message-from-style (quote angles))
 '(message-log-max 1000)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-user-organization "Bloomberg L.P.")
 '(message-wash-forwarded-subjects t)
 '(minibuf-isearch-always-with-complete t)
 '(minibuf-isearch-message-on-right t)
 '(minibuf-isearch-use-migemo nil)
 '(mmm-global-mode (quote maybe) nil (mmm-mode))
 '(mmm-mode-ext-classes-alist (quote ((cperl-mode "" here-doc) (sh-mode "" here-doc) (html-helper-mode "" html-js) (html-helper-mode "" embedded-css))) nil (mmm-mode))
 '(mmm-mode-string "")
 '(mouse-highlight 1)
 '(mouse-wheel-mode t nil (mwheel))
 '(mpd-db-root "/srv/mpd/songs")
 '(nnmail-message-id-cache-file "~/.emacs.d/gnus/nnmail-cache")
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(pgg-default-user-id "\"Ramkumar R\" <andyetitmoves@gmail.com>")
 '(predictive-auto-learn t)
 '(predictive-dict-autosave nil)
 '(ps-use-face-background t)
 '(query-replace-from-history-variable (quote query-replace-from-history))
 '(query-replace-to-history-variable (quote query-replace-to-history))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-save-parse-info t)
 '(reftex-toc-split-windows-horizontally t)
 '(safe-local-variable-values (quote ((c-comment-only-line-offset . 0) (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . 0) (substatement-open . 0) (label . 0) (statement-cont . +)))))
 '(scroll-bar-mode nil)
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-traditional))
 '(semantic-mode t)
 '(semanticdb-default-save-directory "~/.emacs.d/semantic-cache")
 '(semanticdb-default-system-save-directory "~/.emacs.d/semantic-cache")
 '(server-mode t)
 '(session-initialize t nil (session))
 '(session-locals-include (quote (buffer-read-only view-mode buffer-undo-list)))
 '(session-save-file "~/.emacs.d/session-state.el")
 '(session-undo-check -1)
 '(sh-shell-arg (quote ((bash . "-i") (csh . "-f") (pdksh) (ksh88) (rc . "-p") (wksh) (zsh . "-f"))))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(smtpmail-queue-dir "~/.emacs.d/queued-mail/")
 '(ssl-view-certificate-program-arguments (quote ("x509" "-text" "-inform" "DER")))
 '(ssl-view-certificate-program-name "openssl")
 '(svn-status-preserve-window-configuration t)
 '(table-time-before-reformat 0)
 '(table-time-before-update 0)
 '(tempbuf-minimum-timeout 60)
 '(template-auto-update-disable-regexp "\\.el$")
 '(template-default-directories (quote ("~/.emacs.d/templates/")))
 '(template-initialize (quote (auto ffap cc-mode de-html-helper keys)))
 '(thumbs-thumbsdir "~/.emacs.d/thumbs")
 '(tool-bar-mode nil)
 '(tooltip-gud-tips-p t t (tooltip))
 '(tooltip-mode t nil (tooltip))
 '(tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
 '(tramp-backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(tramp-default-method "sftp")
 '(tramp-default-method-alist nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-automatic-caching t)
 '(url-configuration-directory "~/.emacs.d/url")
 '(vc-make-backup-files t)
 '(version-control t)
 '(view-inhibit-help-message t)
 '(view-read-only t)
 '(view-remove-frame-by-deleting t)
 '(visible-bell t)
 '(w3-configuration-directory "~/.emacs.d/w3/")
 '(w3-do-incremental-display t)
 '(w3-honor-stylesheets t)
 '(w3-horizontal-rule-char 45)
 '(w3-use-terminal-characters t)
 '(w3-user-colors-take-precedence t)
 '(windmove-wrap-around t)
 '(x-select-enable-clipboard t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(border ((((class color) (background dark)) (:background "grey70"))))
 '(completions-common-part ((((class color) (background dark)) (:foreground "grey"))))
 '(completions-first-difference ((((class color) (background dark)) (:foreground "coral"))))
 '(cursor ((((class color) (background dark)) (:background "lightgreen"))))
 '(custom-documentation ((t (:inherit font-lock-doc-face))))
 '(custom-variable-button ((t (:inherit (bold underline)))))
 '(escape-glyph ((((background dark)) (:foreground "cyan4"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit escape-glyph))))
 '(font-lock-regexp-grouping-construct ((((class color) (background dark)) (:foreground "yellow"))))
 '(fringe ((((class color) (background dark)) (:background "grey20"))))
 '(gnus-cite-attribution ((t (:inherit italic))))
 '(gnus-emphasis-bold ((t (:inherit bold))))
 '(gnus-emphasis-bold-italic ((t (:inherit (gnus-emphasis-bold gnus-emphasis-italic)))))
 '(gnus-emphasis-highlight-words ((t (:inherit highlight))))
 '(gnus-emphasis-italic ((t (:inherit italic))))
 '(gnus-emphasis-underline-bold ((t (:inherit (gnus-emphasis-underline gnus-emphasis-bold)))))
 '(gnus-emphasis-underline-bold-italic ((t (:inherit (gnus-emphasis-underline gnus-emphasis-bold gnus-emphasis-italic)))))
 '(gnus-emphasis-underline-italic ((t (:inherit (gnus-emphasis-underline gnus-emphasis-italic)))))
 '(gnus-header-content ((((class color) (background dark)) (:inherit italic :foreground "forest green"))))
 '(gnus-header-newsgroups ((((class color) (background dark)) (:inherit italic :foreground "yellow"))))
 '(gnus-signature ((((class color) (background dark)) (:foreground "CadetBlue"))))
 '(header-line ((((class color) (background dark)) (:background "brown4" :foreground "lightpink" :box (:line-width 3 :color "gray20") :width semi-expanded))))
 '(highlight ((((class color) (background dark)) (:background "cyan" :foreground "darkblue"))))
 '(highlight-beyond-fill-column-face ((((class color) (background dark)) (:background "grey20"))))
 '(highlight-changes ((((min-colors 88) (class color) (background dark)) (:foreground "lightgoldenrod"))))
 '(highlight-changes-delete ((((class color) (background dark)) (:foreground "yellow" :underline t))))
 '(highlight-current-line ((((class color) (background dark)) (:background "grey20"))))
 '(ido-first-match ((((class color) (background dark)) (:foreground "yellow"))))
 '(info-node ((((class color) (background dark)) (:inherit bold :foreground "cyan"))))
 '(menu ((((type x-toolkit)) (:background "black" :foreground "cyan"))))
 '(mmm-default-submode-face ((((class color) (background dark)) (:background "gray10"))))
 '(mode-line ((t (:background "black" :foreground "cyan" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((((class color) (background dark)) (:foreground "gold"))))
 '(next-error ((((class color) (background dark)) (:background "yellow" :foreground "red"))))
 '(region ((((class color) (background dark)) (:background "cyan" :foreground "darkblue"))))
 '(rfcview-headname-face ((t (:inherit info-node))))
 '(rfcview-headnum-face ((t (:inherit info-node))))
 '(rfcview-mouseover-face ((t (:inherit highlight))))
 '(rfcview-rfcnum-face ((((class color) (background dark)) (:foreground "orange"))))
 '(rfcview-stdnum-face ((((class color) (background dark)) (:foreground "lightsteelblue"))))
 '(sh-heredoc ((((min-colors 88) (class color) (background dark)) (:inherit bold :foreground "yellow1"))))
 '(show-paren-match ((((class color) (background dark)) (:background "grey28"))))
 '(show-paren-mismatch ((((class color) (background dark)) (:background "lightpink" :foreground "darkblue"))))
 '(table-cell ((((class color) (background dark)) (:background "grey10" :foreground "skyblue"))))
 '(template-message-face ((t (:inherit bold))))
 '(todoo-item-header-face ((((class color) (background dark)) (:inherit bold :foreground "goldenrod"))))
 '(tooltip ((((class color) (background dark)) (:inherit variable-pitch :background "black" :foreground "green"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "grey20")))))

;;;; Font Setup

;;; Terminal specific

(custom-theme-set-faces
 'user
 ;; FIXME: slant roman bug: Uncomment when that gets resolved.
 ;; '(default
 ;;   ((((type tty)) (:background "black" :foreground "white"))))
 '(mode-line
   ((((class color) (background dark) (type tty))
     (:background "gray10" :foreground "cyan"))
    (((class color) (background dark) (type x-toolkit))
     (:foreground "cyan" :box (:line-width -1 :style released-button))))))

;;; Language specific

;; Method:
;; Open gucharmap to find the beginning and ending character codes
;; Locate a font for the language and set

;; FIXME: set-fontset-font definition has changed (is this really needed now?)
;; (when (eq window-system 'x)
;;   (set-fontset-font
;;    nil
;;    (cons (decode-char 'ucs #x0900) (decode-char 'ucs #x097f))
;;    "-unknown-gargi-medium-r-normal--0-0-0-0-p-0-iso10646-1")
;;   (set-fontset-font
;;    nil
;;    (cons (decode-char 'ucs #x0b80) (decode-char 'ucs #x0bff))
;;    "-unknown-tscu times-medium-r-normal--0-0-0-0-p-0-iso10646-1"))

;;;; Disabled

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;; Autoloads

(and (boundp 'generated-autoload-file)
     (setq generated-autoload-file "~/.emacs.d/loaddefs.el"))

(load "~/.emacs.d/loaddefs" t)

;;;; Utility functions

(defun hi-backspace ()
  (interactive)
  (if (equal last-command 'hippie-expand)
      (hippie-expand -1)
    (backward-delete-char-untabify 1)))

(defun smart-home ()
  (interactive)
  (let ((point (point)))
    (back-to-indentation)
    (and (= point (point)) (beginning-of-line))))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun define-keys (map &rest args)
  (while (cadr args)
    (define-key map (car args) (cadr args))
    (setq args (cddr args))))

(defun get-interactivity (sym &optional load)
  (if (fboundp sym)
      (let ((form (symbol-function sym)))
	(cond
	 ((subrp form) nil)
	 ((byte-code-function-p form) (and (>= (length form) 6) (aref form 5)))
	 ((symbolp form) (get-interactivity form))
	 ((eq (car-safe form) 'lambda)
	  (setq form (nthcdr 2 form))
	  (and (stringp (car form)) (setq form (cdr form)))
	  (car-safe (cdr-safe (car form))))
	 ((eq (car-safe form) 'autoload)
	  (when load
	    (load (nth 1 form) (eq load t) t)
	    (get-interactivity sym)))
	 ;; next line is implied by cond anyway
	 ;; ((memq (car-safe form) '(macro mocklisp keymap)) nil)
	 ))))

(defun show-variable (variable)
  (interactive (eval (get-interactivity 'describe-variable t)))
  (let ((buffer-name "*Show Variable*") created
	(help-xref-following t))
    (with-current-buffer
	(or (get-buffer buffer-name)
	    (progn
	      (setq created t)
	      (generate-new-buffer buffer-name)))
      (and created (run-hooks 'show-variable-buffer-init-hook))
      (describe-variable variable (current-buffer)))))

;;;; Bindings

(pc-bindings-mode)

(define-keys (current-global-map)
  [remap backward-delete-char-untabify]	'hi-backspace
  [remap beginning-of-line]		'smart-home
  [remap newline]			'reindent-then-newline-and-indent
  [?\t]					'hippie-expand
  [C-f3]				'find-grep-dired
  [f4]					'kill-this-buffer
  [(control ?x) ?k]			'kill-this-buffer
  [(control f5)]			'font-lock-fontify-buffer
  [f7]					'compile
  [(control ?`)]			'indent-according-to-mode
  [insert]				'back-to-indentation
  [M-insert]				'overwrite-mode
  [(control ?n)]			'open-line
  [(control ?o)]			'other-window
  [C-escape]				'switch-to-other-buffer
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
  [(meta ?d)]				'dictionary-lookup-definition
  [XF86Back]				'undo
  [XF86Forward]				'redo
  [(control super up)]			'windmove-up
  [(control super down)]		'windmove-down
  [(control super left)]		'windmove-left
  [(control super right)]		'windmove-right
  )

(eval-after-load "empi"
  '(progn
     (global-set-key [(control e)] empi-map)
     (define-key empi-map [(control e)] 'move-end-of-line)))

;; (require 'ffap)
;; (eval (cons 'progn (cons '(global-set-key [M-S-mouse-3] 'ffap-at-mouse)
;; 			 (cdr ffap-bindings))))

(define-keys emacs-lisp-mode-map
  [?\t]			'hippie-expand
  [f8]			'emacs-lisp-byte-compile
  [(control f8)]	'emacs-lisp-byte-compile-and-load
  [f9]			'edebug-defun
  )

(defvar view-mode-map)
(defun view-after-load-hook ()
  ;; [return] causes incorrect overriding with help-mode
  (define-key view-mode-map [return] 'source-jump-at-point)
  (define-key view-mode-map [mouse-2] 'source-jump-at-mouse)
  ;; Our view-mode clobbers mouse-2 as well.
  ;; Emacs-23 doesn't seem to have the override-view-map below, commenting out.
  ;; (eval-after-load "help"
  ;;   '(progn
  ;;      (define-key help-xref-override-view-map [mouse-2] 'help-follow-mouse)
  ;;      (define-key help-xref-override-view-map [return] nil)))
  (define-key view-mode-map [backspace] 'pop-global-mark))

(eval-after-load "view" '(view-after-load-hook))

(eval-after-load "tex"
  '(define-keys TeX-mode-map
     [(control return)]	'reftex-view-crossref))

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (local-set-key [remap fill-paragraph] 'LaTeX-fill-paragraph)))

;;; isearch quits on backspace, if not for the code below
;;; due to a global key binding to hi-backspace above.
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

(eval-after-load "minibuf-isearch"
  '(define-key minibuf-isearch-mode-map [backspace] 'minibuf-isearch-backspace))

(add-hook 'ido-setup-hook '(lambda ()
			     (define-keys ido-completion-map
			       [up]		'ido-prev-work-directory
			       [down]		'ido-next-work-directory
			       [(meta up)]	'ido-prev-match-dir
			       [(meta down)]	'ido-next-match-dir)))

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

(defvar dictionary-mode-map)
(defun dictionary-after-load-hook ()
  (define-key dictionary-mode-map [mouse-1] 'link-mouse-click)
  (add-hook 'dictionary-mode-hook
	    '(lambda ()
	       (define-key view-mode-map [return] 'link-selected))))

(eval-after-load "dictionary" '(dictionary-after-load-hook))

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
(add-to-list 'auto-mode-alist '("\\.tpl$" nil t))

(eval-when-compile (require 'sh-script))

(defun shell-zsh-mode ()
  (sh-mode)
  (sh-set-shell "zsh"))

(defun shell-zsh-script-mode ()
  (interactive)
  (shell-zsh-mode)
  (setq sh-basic-offset 2))

(add-to-list 'magic-mode-alist
	     '("#\\(?:autoload\\|compdef\\)" . shell-zsh-script-mode))

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

(defalias 'perl-mode 'cperl-mode)

;;;; File / Mode hooks

;; (add-hook 'find-file-hooks 'shell-conv-find-file-hook)

;;;(add-hook 'find-file-hooks 'autovc-find-file-hook)

(eval-when-compile (require 'cc-mode))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-toggle-auto-hungry-state 1)
	    (define-key c-mode-base-map [?\t] 'hippie-expand)
	    (highlight-beyond-fill-column)))
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 8)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (if (and buffer-file-name
	     (string-equal
	      "elc" (downcase (or (file-name-extension buffer-file-name) ""))))
	(toggle-read-only 1)
      (highlight-beyond-fill-column)
      (turn-on-auto-fill))))

(defun disable-trw ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'term-mode-hook 'disable-trw)
(add-hook 'custom-mode-hook 'disable-trw)
(add-hook 'w3-mode-hook 'disable-trw)
(add-hook 'gnus-group-mode-hook 'disable-trw)

(add-hook 'gnus-article-mode-hook
	  '(lambda ()
	     (setq truncate-lines nil)
	     (setq show-trailing-whitespace nil)))

(add-hook 'message-mode-hook
	  '(lambda ()
	     (setq fill-column 68)
	     (setq truncate-lines nil)
	     (setq show-trailing-whitespace nil)))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

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
    (require 'regexp-opt)
    (regexp-opt '("*Message*" "*Messages*" "*tramp"))))

(add-hook 'ido-make-buffer-list-hook
	  '(lambda ()
	     (setq ido-temp-list
		   (filter-list
		    '(lambda (item)
		       (not (string-match selective-buflist-exclusion-regexp
					  item))) ido-temp-list))))

(defun user-buffer-list ()
  (let (buflist)
    (mapc '(lambda (item)
	     (or (string= (substring (buffer-name item) 0 1) " ")
		 (setq buflist (cons item buflist))))
	  (buffer-list)) buflist))

(global-set-key [(control tab)] 'ibs-select)
(defvar ibs-cycle-buffer-function)
(setq ibs-cycle-buffer-function 'user-buffer-list)

(defvar tempbuf-exclusion-regexp
  (eval-when-compile
    (require 'regexp-opt)
    (regexp-opt '("*Message*" "*Messages*" "*scratch*" "*Echo Area"
		  "*Minibuf-" "*info tag table*" "*empl-buffer*"))))

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

;(run-with-idle-timer 2 t 'make-all-temp-bufs)

(defalias 'orig-quit-window (symbol-function 'quit-window))
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
	 (not (and orig (boundp orig)
		   (equal (symbol-value orig) (symbol-value var)))))))

(defun session-before-store-buffer ()
  (let ((cur buffer-undo-list) last)
    (while cur
      (if (and (listp cur) (listp (car cur))
	       (or (markerp (caar cur))
		   (and (eq (caar cur) 'apply)
			(condition-case nil
			    (progn
			      (read (prin1-to-string (car cur))) nil)
			  (error t)))))
	  (if last
	      (setcdr last (cdr cur))
	    (setq buffer-undo-list (cdr buffer-undo-list)))
	(setq last cur))
      (setq cur (cdr-safe cur)))))

(eval-when-compile (require 'session))

(setq session-locals-predicate 'session-locals-predicate)

(setq mmm-global-classes '(universal file-variables))

;;;; Files

(defvar read-only-file-patterns '())

(defun make-specific-files-read-only ()
  (let ((patterns read-only-file-patterns) (case-fold-search nil))
    (catch 'pattern-matched
      (while patterns
	(and (stringp (car patterns))
	     (string-match (car patterns) buffer-file-name)
	     (toggle-read-only 1)
	     (throw 'pattern-matched nil))
	(setq patterns (cdr patterns))))))

(add-hook 'find-file-hooks 'make-specific-files-read-only)

(require 'proj)
(template-initialize)

(defvar backup-exclude-dirs
  (list temporary-file-directory small-temporary-file-directory))

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

; (eval-when-compile (require 'tramp))

(defun make-backup-file-name-rebase-directory (file)
  (if (memq system-type '(windows-nt ms-dos cygwin vax-vms axp-vms))
      ;; Steer clear of other OS's
      (let (make-backup-file-name-function)
	(make-backup-file-name file))
    (let ((alist backup-directory-alist) elt backup-directory rootdir)
      (while alist
	(setq elt (car alist))
	(if (string-match (car elt) file)
	    (setq backup-directory (cdr elt)
		  alist nil)
	  (setq alist (cdr alist))))
      (or (file-name-absolute-p file)
	  (setq file (expand-file-name file)))
      (setq rootdir
	    (if (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p file))
		(with-parsed-tramp-file-name file nil
		  (tramp-make-tramp-file-name
		   method user host "/"))
	      "/"))
      (or (file-name-absolute-p file)
	  (setq file (expand-file-name file)))
      (setq backup-directory
	    (expand-file-name
	     (file-relative-name
	      (file-name-directory file) rootdir) backup-directory))
      (condition-case nil
	  (make-directory backup-directory 'parents)
	(file-error (setq backup-directory nil)))
      (if backup-directory
	  (concat (expand-file-name (file-name-nondirectory file)
				    backup-directory) "~") file))))

(setq make-backup-file-name-function 'make-backup-file-name-rebase-directory)

(defadvice find-function-noselect
  (before strip-definition-advice (function) activate preactivate)
  "Modify `find-function' to accomodate advised subrs"
  (when (and (symbolp function) (ad-is-advised function))
    (let ((def (ad-get-orig-definition function)))
      (when (and def (or (eq (car-safe def) 'autoload) (subrp def)))
	(setq function (make-symbol (symbol-name function)))
	(fset function def)))))

;;;; Specific package init

;; Silence the compiler
(defvar dired-deletion-confirmer)
(setq dired-deletion-confirmer 'y-or-n-p)

(eval-when-compile (defvar tooltip-gud-tips-p))
(and window-system (setq tooltip-gud-tips-p t))

(setq compilation-environment
      '("CPPFLAGS=" "CFLAGS=-march=athlon-xp -mfpmath=sse -ggdb3"
	"CXXFLAGS=-march=athlon-xp -mfpmath=sse -ggdb3" "LDFLAGS="))

(defun doxymacs-add-project (name)
  (let ((home (expand-file-name "~")))
    (add-to-list 'doxymacs-doxygen-dirs
		 (list (concat "^" home "/programs/" name "/")
		       (concat home "/.doxygen.tags/" name ".xml")
		       (concat "file://" home "/programs/" name "/doc/")))))

(eval-when-compile (defvar mpg123-startup-volume))
(setq mpg123-startup-volume nil)

;; Shut up Mr. Compiler!
(defvar tramp-methods)

(defun tramp-methods-parameter-set (method param value)
  (let ((valpos (cdr-safe (assq param (cdr (assoc method tramp-methods))))))
    (and (consp valpos) (setcar valpos value))))

(eval-after-load "tramp"
  '(progn
     (tramp-methods-parameter-set "su" 'tramp-login-args '("%u" "-s" "/bin/sh"))
     (tramp-methods-parameter-set
      "sudo" 'tramp-login-args '("-u" "%u" "-p" "Password:" "/bin/sh"))))

(eval-after-load "rfcview" '(require 'info))

;; For with-unlogged-message
(eval-when-compile (require 'empi-core))

(defvar fortune-last)

(defun display-fortune ()
  (interactive)
  (with-temp-buffer
    (call-process "/usr/games/fortune" nil t nil "-a")
    (setq fortune-last (buffer-string))
    (with-unlogged-message (display-message-or-buffer fortune-last))))

(run-with-idle-timer 60 t 'display-fortune)

;;;; After init

(add-hook 'after-init-hook
	  '(lambda ()
	     (add-hook 'find-file-hook 'save-protected-buffer-variables t)
	     (add-hook 'kill-buffer-hook 'session-before-store-buffer)
	     (setq safe-load-compile-end-prompt nil)))

(when (and (eq window-system 'x)
	   (locate-library "imenu"))
  (add-hook 'semantic-init-hooks
	    (lambda ()
	      (condition-case nil
		  (imenu-add-to-menubar "TAGS")
		(error nil)))))

(require 'files-patch)

(put 'upcase-region 'disabled nil)
