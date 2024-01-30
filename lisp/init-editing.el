;; ** vim editing
(require 'init-evil)

;; ** note taking (org, md, etc)
;; ** narrow window
;; ** format
;; ** others

;; ;; highlight TODO
;; (use-package hl-todo
;;   :ensure t
;;   :defer t
;;   :hook (prog-mode . hl-todo-mode)
;;   :config
;;   (defvar hl-todo-keyword-faces
;;     `(("TODO"       warning bold)
;;       ("FIXME"      error bold)
;;       ("HACK"       font-lock-constant-face bold)
;;       ("REVIEW"     font-lock-keyword-face bold)
;;       ("NOTE"       success bold)
;;       ("DEPRECATED" font-lock-doc-face bold))
;;     )
;;   )

;; yasnippet config
(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path
              "~/.config/emacs/plugins/yasnippet")
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; auto-yasnippet
(use-package auto-yasnippet
  :ensure t
  :after (yasnippet)
  :defer t
  :config
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand)
  )

;; ;; undo and redo
;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (turn-on-undo-tree-mode)
;;   ;; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
;;   )

;; end
(provide 'init-editing)
