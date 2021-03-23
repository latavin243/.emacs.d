;;; package --- Summary
;;; Commentary:
;;; Code:

;; disable startup page
(setq inhibit-startup-screen t)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; disable other useless menus
(unless (eq window-system 'ns) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; line number
(global-linum-mode t)
(setq linum-format "%d ")
(set-face-background 'linum nil)

;; cursor type
;; (setq cursor-type 'bar)

;; auto load outer change
(global-auto-revert-mode t)

;; ;; abbreviation
;; (setq-default abbrev-mode t)
;; (define-abbrev-table 'global-abbrev-table
;;   '(
;;     ("abbr" "abbreviation")
;;     ))

;; no backup files
(setq make-backup-files nil)

;; disable autosave
(setq auto-save-default nil)

;; recent file
(recentf-mode 1)
(defvar recentf-max-menu-items 25)

;; highlight matching brackets
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; show recent files
(recentf-mode t)

;; delete selected content
(delete-selection-mode t)

;; better hippie complete
(defvar hippie-expand-try-function-list '(try-expand-debbrev
					                      try-expand-debbrev-all-buffers
					                      try-expand-debbrev-from-kill
					                      try-complete-file-name-partially
					                      try-complete-file-name
					                      try-expand-all-abbrevs
					                      try-expand-list
					                      try-expand-line
					                      try-complete-lisp-symbol-partially
					                      try-complete-lisp-symbol))

;; use y-n instead of yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; dired mode recursively delete and copy
(defvar dired-recursive-copies 'always)
(defvar dired-recursive-deletes 'always)

;; dired mode config
(put 'dired-find-alternate-file 'disabled nil)

;; dired-x, c-x c-j to enter current dir in dired-mode
(require 'dired-x)
(setq dired-dwim-target t)

;; show paren inside one
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	    (t (save-excursion
	         (ignore-errors (backward-up-list))
	         (funcall fn)))))

;; handle dos eol (^M)
(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; better occur mode
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	        (buffer-substring-no-properties
	         (region-beginning)
	         (region-end))
	      (let ((sym (thing-at-point 'symbol)))
	        (when (stringp sym)
	          (regexp-quote sym))))
	    regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

;; encoding to utf-8
(set-language-environment "UTF-8")

;; open config file command
(defun open-init-file()
  "Opens init.el config."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 4 spaces to replace table
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; narrow region setup
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; scroll config
(setq scroll-step 1 scroll-margin 3 scroll-conservatively 10000)

;; confirm kill
;; (setq confirm-kill-emacs
;;       (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y")))

;; better gc
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
