;; undo and redo
(use-package undo-fu)

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

;; iedit, multi editing
(use-package iedit
  ;; default key binding: C-;
  )

;; end of package
(provide 'init-editing)
