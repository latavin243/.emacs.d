;;; init-package.el -- package management
;;; Commentary:
;;; package config
;;; Code:

;; important packages
;; vim related
(straight-use-package 'window-numbering)
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

;; counsel-imenu
(global-set-key (kbd "M-s i") 'counsel-imenu)

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

;; treemacs, sidebar explorer
(use-package treemacs
  :ensure t
  :after (evil-leader)
  :init
  (evil-leader/set-key
    "ee" 'treemacs
    )
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  :config
  (evil-define-key 'treemacs treemacs-mode-map (kbd "RET") #'treemacs-root-down)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "DEL") #'treemacs-root-up)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-RET-action)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") #'treemacs-TAB-action)
  )

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  )

;; (use-package treemacs-icons-dired
;;   :ensure t
;;   :after (treemacs dired)
;;   :config
;;   (treemacs-icons-dired-mode)
;;   )

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  )

;; jump window like tmux prefix-q

;; (use-package switch-window
;;   :ensure t
;;   :config
;;   ;; (setq switch-window-shortcut-appearance 'asciiart)
;;   (evil-leader/set-key
;;     "qq" 'switch-window
;;     "ww" 'switch-window
;;     ))

(use-package ace-window
  :ensure t
  :after (evil-leader)
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (evil-leader/set-key
    "qq" 'ace-window
    "ww" 'ace-window
    ))


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

;; ;; nyan cat
;; (use-package nyan-mode
;;   :ensure t
;;   :init (setq
;;          nyan-animate-nyancat t
;;   	     nyan-bar-length 20
;;          nyan-wavy-trail t
;;          )
;;   :hook ((after-init . nyan-mode))
;;   )

;; restart emacs
(use-package restart-emacs
  :ensure t
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

;; doom modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

;; startup dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Happy Hacking")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  ;;(setq dashboard-show-shortcuts nil)
  (setq dashboard-items '(
                          (recents  . 20)
                          (bookmarks . 5)
                          (projects . 10)
                          ;;(agenda . 5)
                          ;;(registers . 5)
                          ))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(use-package auto-highlight-symbol
  :ensure t
  )

(use-package ace-jump-mode
  :ensure t
  :after (evil)
  :config
  (evil-leader/set-key
    "ss" 'ace-jump-mode
    )
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
