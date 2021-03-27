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

;; subword, enable jump camelCase word
(use-package subword
  :diminish subword-mode
  :init
  (global-subword-mode)
  )

(provide 'init-code-navigation)
