;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("Melpa" . "https://melpa.org/packages/") t)

;; * 0. package manager
;; ** use-package
(require 'use-package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)

;; * 1. basic
(require 'init-defaults)
(require 'init-basic)

;; * 2. decoration
;; ** welcome page
;; ** floatwindow
;; ** bottom bar
;; ** theme
(require 'init-decoration)

;; * 3. navigation
;; ** file search
;; ** code search
;; ** sidebar file explorer
;; ** project swtich
;; ** window jump
;; ** buffer / tab jump
;; ** code jump
;; ** file browser
(require 'init-navigation)
(require 'init-treemacs)
(require 'init-swiper)
(use-package window-numbering)

;; * 4. editing
;; ** vim editing
;; ** note taking (org, md, etc)
;; ** narrow window
;; ** format
;; ** others
(require 'init-evil)
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
;; yasnippet config
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
(use-package undo-tree
  :ensure t
  :config
  (turn-on-undo-tree-mode)
  ;; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  )


;; * 5. coding & language
;; ** code syntax, completion
;; ** spell check
;; ** code format
;; ** snippets
;; ** playground
;; ** runner
(require 'init-language)
(require 'init-org)

;; * 6. dev tools
;; ** git
;; ** terminal
;; ** copilot
(require 'init-git)

;; TODO
;; (require 'init-packages)

;; load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
