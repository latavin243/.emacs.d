;; flycheck
(use-package flycheck
  :after (evil)
  :init
  (global-flycheck-mode)
  :config
  (evil-define-key 'normal 'global
    "[g" 'flycheck-previous-error
    "]g" 'flycheck-next-error
  )
  )

(provide 'init-check-syntax)
