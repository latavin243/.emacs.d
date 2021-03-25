;; undo and redo
(use-package undo-tree
  :config
  (turn-on-undo-tree-mode)
  ;; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  )

;; which key
(use-package which-key
  :config
  (which-key-mode)
  )

;; smartparens config
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(use-package smartparens
  :config
  (smartparens-global-mode t)
  )

;; expand region config
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

;; end of package
(provide 'init-editing)
