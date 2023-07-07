;; magit
(use-package magit
  :ensure t
  )

;; git gutter
(use-package git-gutter+
  :ensure t
  :config
  (progn (global-git-gutter+-mode))
  )

(provide 'init-git)
