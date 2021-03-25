;; jump to declaration
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package ace-jump-mode
  :ensure t
  :after (evil)
  :config
  (evil-leader/set-key
    "ss" 'ace-jump-mode
    )
  )

(provide 'init-code-jump)
