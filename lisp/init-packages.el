;;; init-package.el -- package management
;;; Commentary:
;;; package config
;;; Code:

;; important packages

;; others
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'reveal-in-osx-finder)

;; cl - Common Lisp Extension
(require 'cl)
(require 'loadhist)
(file-dependents (feature-file 'cl))

;; exec path from shell
;; Find Executable Path on OS X
(when (memq window-system '(mac ns)) (exec-path-from-shell-initialize))
(defvar exec-path-from-shell-check-startup-files nil)


;; smartparens config
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(straight-use-package 'smartparens)
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  )

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; company mode
(use-package company
  :ensure t
  :init
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   )
  :config
  (global-company-mode)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  )

;; popwin config
(use-package popwin
  :ensure t
  :config
  (popwin-mode t)
  )

;; expand region config
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

;; iedit
(straight-use-package 'iedit)
;; (global-set-key (kbd "M-s e") 'iedit-mode)
;; c-; will do

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  )

;; yasnippet config
(straight-use-package 'yasnippet)
(use-package yasnippet
  :ensure t
  :config
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

;; undo and redo
(straight-use-package 'undo-tree)
(use-package undo-tree
  :ensure t
  :config
  (turn-on-undo-tree-mode)
  ;; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  )

;; which key
(straight-use-package 'which-key)
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

;; auto package update
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  )

;; json
(use-package json-mode
  :ensure t
  )

;; yaml
(use-package yaml-mode
  :ensure t
  )

;; pb
(use-package protobuf-mode
  :ensure t
  )

;; (use-package company-lsp
;;   :ensure t
;;   :defer t
;;   :init (setq company-lsp-cache-candidates 'auto))

;; jump to declearation
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

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

(use-package auto-highlight-symbol
  :ensure t
  )

(straight-use-package 'projectile)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; highlight TODO
(use-package hl-todo
  :ensure t
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

;; (use-package flyspell
;;   :ensure t
;;   )

;; (use-package wucuo
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'wucuo-start)
;;   (add-hook 'text-mode-hook #'wucuo-start)
;;   )

(provide 'init-packages)
;;; init-packages.el ends here
