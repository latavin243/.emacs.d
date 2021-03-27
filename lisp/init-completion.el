;; company mode
(use-package company
  :init
  (global-company-mode)
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   )
  :config
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  )

;; company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

;; lsp
(use-package lsp-mode
  :hook (go-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  )

(provide 'init-completion)
