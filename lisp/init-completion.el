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

;; lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  )

(provide 'init-completion)
