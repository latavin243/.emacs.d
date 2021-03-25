;; highlight symbol
(use-package auto-highlight-symbol)

;; highlight TODO
(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (defvar hl-todo-keyword-faces
    `(("TODO"       warning bold)
      ("FIXME"      error bold)
      ("HACK"       font-lock-constant-face bold)
      ("REVIEW"     font-lock-keyword-face bold)
      ("NOTE"       success bold)
      ("DEPRECATED" font-lock-doc-face bold))
    )
  )

;; end of package
(provide 'init-highlight)
