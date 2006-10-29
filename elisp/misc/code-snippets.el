;;; CODE-SNIPPETS.EL --- Some miscellaneous code pieces

(eval-and-compile
  (error "This file is meant for browsing only, don't compile or load"))

(defconst perldoc-pager-var "PERLDOC_PAGER")
(defconst perldoc-req-pager "cat")
(defvar perldoc-orig-pager nil)
(defvar perldoc-env-push-count 0)
(defun perldoc-push-env ()
  (setq perldoc-env-push-count (1+ perldoc-env-push-count))
  (when (= perldoc-env-push-count 0)
    (setq perldoc-orig-pager (getenv perldoc-pager-var))
    (setenv perldoc-pager-var perldoc-req-pager)))
(defun perldoc-pop-env ()
  (when (> perldoc-env-push-count 0)
    (set perldoc-env-push-count (1- perldoc-env-push-count))
    (if (= perldoc-env-push-count 0)
	(setenv perldoc-pager-var perldoc-orig-pager))))

(defmacro break-scan (list test &rest fail)
  `(let ((break-scan-item ,list))
     (catch 'break-scan-exit
       (while break-scan-item
	 (and (,test (car break-scan-item))
	      (throw 'break-scan-exit break-scan-item))
	 (setq break-scan-item (cdr break-scan-item)))
       ,@fail)))

(defsubst echo-cmd (cmd)
  (with-temp-buffer
    (shell-command cmd t)
    (message "%s" (buffer-string))))

(eval-when-compile
  (eval
   `(defmacro with-unlogged-message (&rest args)
      (cons 'let
	    (append
	     (list
	      ,(if (boundp 'log-message-filter-function)
		   '(list '(log-message-filter-function #'ignore))
		 '(list '(message-log-max nil))))
	     args)))))

(defun compare-file-times (f1 f2)
  "Compare files F1 and F2 based on their last modified timestamps.
Return nil on failure, -1 if F1 is older, 0 if both are of the same age and 1 if
F1 is newer."
  (let ((lof1 (nth 5 (file-attributes f1))) hif1
	(lof2 (nth 5 (file-attributes f2))) hif2)
    (when (and lof1 lof2)
      (setq hif1 (car lof1))
      (setq lof1 (cadr lof1))
      (setq hif2 (car lof2))
      (setq lof2 (cadr lof2))
      (if (< hif1 hif2) -1
	(if (= hif1 hif2)
	    (if (< lof1 lof2) -1 (if (= lof1 lof2) 0 1)) 1)))))

(defun randcharstr () (char-to-string (+ (random 95) 32)))

(defun randstr (len)
  (let ((i len) (str ""))
    (while (> len 0)
      (setq str (concat str (randcharstr)))
      (setq len (1- len))) str))

(defun test-title-length (&optional prefix)
  (interactive "sEnter prefix: ")
  (or prefix (setq prefix ""))
  (let ((test-str (concat prefix (randcharstr))) (resok -1) (totlen 0) (tries 0)
	minlen (maxlen 0) (lname (frame-parameter nil 'name)))
    (set-frame-name "")
    (condition-case nil
	(while t
	  (while (< resok 2)
	    (set-frame-name test-str)
	    (if (y-or-n-p (concat "Can you see till \""
				  (if (> (length test-str) 5)
				      (concat "..." (substring test-str -5))
				    test-str) "\" "))
		(if (= resok 1) (setq resok 2) (if (= resok -1) (setq resok 0)))
	      (if (= resok 0) (setq resok 3) (if (= resok -1) (setq resok 1))))
	    (and (not (= resok 2))
		 (setq test-str
		       (if (= resok 0)
			   (concat test-str (randcharstr))
			 (if (string= test-str "")
			     (error "Display area too small!")
			   (substring test-str 0 -1))))))
	  (setq resok (length test-str))
	  (setq totlen (+ totlen resok))
	  (setq tries (1+ tries))
	  (and (or (not minlen) (< resok minlen)) (setq minlen resok))
	  (and (> resok maxlen) (setq maxlen resok))
	  (if (< resok (length prefix))
	      (error "Prefix too long, try with a shorter one")
	    (setq test-str (concat prefix (randstr (- resok (length prefix))))))
	  (setq resok -1))
      (quit
       (if (> tries 0)
	   (message "Number of samples analyzed: %d
The maximum title length was %d.
The minimum was %d.\nMaybe you should rely on %d."
		    tries maxlen minlen (/ totlen tries))
	 (message "You should have carried on, try again!"))))
    (set-frame-name lname)))

(defun buffer-what-changed (&optional buffer)
  (interactive)
  (and buffer (set-buffer buffer))
  (let ((file (buffer-file-name)) (newbuf (current-buffer)))
    (if file
	(setq file (file-truename file))
      (error "Buffer specified does not visit a file"))
    (set-buffer (generate-new-buffer
		 (format "*%s-original*" (buffer-name newbuf))))
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (normal-mode t))
    (set-buffer-modified-p nil)
    (toggle-read-only 1)
    (ediff-buffers (current-buffer) newbuf)))

(eval-and-compile (defvar dynlog-debug nil))
(eval-and-compile
  (if dynlog-debug
      (defmacro make-func-timer (inter func lazy) t)
    (defsubst make-func-timer (inter func lazy)
      (funcall (if lazy 'run-with-idle-timer 'run-at-time) inter inter func))))

(defun fsk-plist-keys (sym)
  (let ((rest (symbol-plist sym)) keys)
    (while rest
      (setq keys (cons (car rest) keys))
      (setq rest (cddr rest)))
    keys))

(defun find-sym-keys ()
  (let (allkeys)
    (mapatoms '(lambda (sym) (setq allkeys (union allkeys (fsk-plist-keys sym)))))
    (insert (mapconcat '(lambda (item) (format "%s" item))
		       (sort allkeys '(lambda (s1 s2)
					(string-lessp (symbol-name s1)
						      (symbol-name s2)))) "\n"))))

(defun find-sym-having-key (key)
  (interactive
   (list (intern-soft (completing-read "Enter key: " obarray
				       nil t (current-word)))))
  (insert (format "%S :-\n\n" key))
  (mapatoms '(lambda (sym)
	       (and (plist-member (symbol-plist sym) key)
		    (insert (format "%S: %S\n" sym (get sym key)))))))

(defun find-emacs-caps ()
  (interactive)
  (let (special-forms builtins def (buf (get-buffer-create "*Emacs Caps*")))
    (mapatoms '(lambda (sym)
		 (and (fboundp sym)
		      (subrp (setq def (symbol-function sym)))
		      (if (eq 'unevalled (cdr (subr-arity def)))
			  (setq special-forms (cons sym special-forms))
			(setq builtins (cons sym builtins))))))
    (with-current-buffer buf
      (toggle-read-only -1)
      (erase-buffer)
      (insert "Special Forms:\n\n"
	      (mapconcat '(lambda (sym) (concat "\t" (symbol-name sym)))
			 (sort special-forms 'string-lessp) "\n")
	      "\n\nBuiltins:\n\n"
	      (mapconcat '(lambda (sym) (concat "\t" (symbol-name sym)))
			 (sort builtins 'string-lessp) "\n") "\n")
      (goto-char 1)
      (set-buffer-modified-p nil)
      (toggle-read-only 1))
    (display-buffer buf)))

(defun check-set (vect idx val &optional refill)
  "Similar to `aset', but with a refilling option.
Call REFILL with VECT if the position to set already has a non-nil value."
  (and (functionp refill) (aref vect idx) (funcall refill vect))
  (aset vect idx val))

(defsubst vput (vect desc key val &optional refill)
  "Set element in sequence based on description sequence and key.
Set the first element in VECT, for which the corresponding element in DESC
matches KEY, to VAL, calling REFILL with VECT if the element at that position is
already non-nil.  Return non-nil if the search succeeds."
  (let ((offset 0))
    (while (and (< offset (length desc)) (< offset (length vect))
		(setq offset
		      (if (not (string= (aref desc offset) key))
			  (1+ offset)
			(check-set vect offset val refill) nil))))
    (not offset)))

(defun print-key (key)
  (interactive "kType key sequence: ")
  (message "%S" key))

(defun get-interactively (str)
  (call-interactively `(lambda (&rest args) (interactive ,str) args)))

(defun filter-list-in-place (func list)
  (let ((item list) (last))
    (while item
      (if (funcall func (car item))
	  (setq last item)
	(if last
	    (setcdr last (cddr last))
	  (setq list (cdr list))))
      (setq item (cdr item)))) list)

(defun filter-list (func list)
  (let (ret (item list))
    (while item
      (and (funcall func (car item))
	   (setq ret (cons (car item) ret)))
      (setq item (cdr item)))
    ret))

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

(defun widget-numrange-validate (widget val)
  (let ((check))
    (and (widget-restricted-sexp-match widget val)
	 (or (not (numberp (setq check (widget-get widget :minimum))))
	     (>= val check))
	 (or (not (numberp (setq check (widget-get widget :maximum))))
	     (<= val check)))))

(define-widget 'range 'restricted-sexp
  "Range of values"
  :tag "Range" :value 0 :size 10
  :match 'widget-numrange-validate)

(defun find-key-field (key desc)
  (let ((offset 0) (len (length desc)))
    (while (and (< offset len) (not (string= (aref desc offset) key)))
      (setq offset (1+ offset)))
    (and (not (= offset len)) offset)))

(defsubst vput (vect desc key val)
  "Set element in sequence based on description sequence and key.
Set the first element in VECT, for which the corresponding element in DESC
matches KEY, to VAL, calling REFILL with VECT if the element at that position is
already non-nil.  Return non-nil if the search succeeds."
  (let ((offset (find-key-field key desc)))
    (and offset (aset vect offset val))))

(defun load-all ()
  "Load all features of emacs."
  (interactive)
  (mapatoms
   '(lambda (atom)
      (let ((def (and (fboundp atom) (symbol-function atom))) file-name)
	(and (eq (car-safe def) 'autoload)
	     (setq file-name (nth 1 def)))
	(and file-name (condition-case nil (load file-name t) (error t)))))))

;;; CODE-SNIPPETS.EL ends here
