;; restart emacs
(use-package restart-emacs
  :ensure t
  )

(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; end
(provide 'init-basic)
