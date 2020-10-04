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

(provide 'init-better-defaults)
