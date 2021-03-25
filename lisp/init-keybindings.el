;; recentf mode
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; find source
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; open init file
(global-set-key (kbd "<f2>") 'open-init-file)

;; hippie complete
(global-set-key (kbd "s-/") 'hippie-expand)

;; lazy load dired mode kbd
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; use c-p and c-n in company completion
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; c-w to kill word
(global-set-key (kbd "C-w") 'backward-kill-word)

;; file fin
(provide 'init-keybindings)
