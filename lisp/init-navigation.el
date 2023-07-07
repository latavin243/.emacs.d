(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package auto-highlight-symbol
  :ensure t
  )

(use-package ace-jump-mode
  :ensure t
  :after (evil)
  :config
  (evil-leader/set-key
    "ss" 'ace-jump-mode
    )
  )

;; jump to declearation
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )


(use-package ace-window
  :ensure t
  :after (evil-leader)
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (evil-leader/set-key
    "qq" 'ace-window
    "ww" 'ace-window
    ))


(provide 'init-navigation)
