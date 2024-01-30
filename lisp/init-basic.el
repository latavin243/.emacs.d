;; restart emacs
(use-package restart-emacs
  :ensure t
)

(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
)

;; (use-package vertico
;;   :ensure t
;;   :config
;;   (vertico-mode t)
;; )

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless))
)

;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode t)
;; )

(use-package embark
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'embark-act)
  (setq prefix-help-command 'embark-prefix-help-command)
)

;; end
(provide 'init-basic)
