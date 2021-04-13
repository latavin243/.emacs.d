(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (evil-leader/set-key
    "pp" 'projectile-switch-project
  )
  )

(provide 'init-projectile)
