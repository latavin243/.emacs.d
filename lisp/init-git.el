;; magit
(use-package magit
  )

;; git gutter
(use-package git-gutter+
  :config
  (progn (global-git-gutter+-mode))
  )

(provide 'init-git)
