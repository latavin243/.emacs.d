;; disable ring bell
(setq ring-bell-function 'ignore)

;; show line number
(global-linum-mode t)

;; auto load outer change
(global-auto-revert-mode t)

;; abbreviation
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table
  '(
    ("abbr" "abbreviation")
    ))

;; no backup files
(setq make-backup-files nil)

;; disable autosave
(setq auto-save-default nil)

;; recent file
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; highlight matching brackets
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; show recent files
(recentf-mode t)

;; delete selected content
(delete-selection-mode t)

;; better hippie complete
(setq hippie-expand-try-function-list '(try-expand-debbrev
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
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired mode config
(put 'dired-find-alternate-file 'disabled nil)

;; dired-x, c-x c-j to enter current dir in dired-mode
(require 'dired-x)
(setq dired-dwim-target t)

(provide 'init-better-defaults)
