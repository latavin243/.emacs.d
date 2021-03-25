;; yasnippet config
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; auto-yasnippet
(use-package auto-yasnippet
  :after (yasnippet)
  :defer t
  :config
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand)
  )

(provide 'init-snippet)
