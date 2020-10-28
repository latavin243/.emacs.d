;;; init-package.el -- package management
;;; Commentary:
;; latavin's Emacs package config
;;; Code:

;; use-package
(straight-use-package 'use-package)
(defvar use-package-always-ensure t)
(custom-set-variables '(package-selected-packages (quote (use-package))))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			             ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; important packages
(straight-use-package 'counsel)
(straight-use-package 'yasnippet)
(straight-use-package 'auto-yasnippet)
;; theme
(straight-use-package 'srcery-theme)
;; vim related
(straight-use-package 'window-numbering)
(straight-use-package 'powerline-evil)
(straight-use-package 'evil-nerd-commenter)
;; others
(straight-use-package 'js2-mode)
(straight-use-package 'nodejs-repl)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'js2-refactor)
(straight-use-package 'web-mode)
(straight-use-package 'reveal-in-osx-finder)
(straight-use-package 'org-pomodoro)

;; cl - Common Lisp Extension
(require 'cl)
(require 'loadhist)
(file-dependents (feature-file 'cl))

;; exec path from shell
;; Find Executable Path on OS X
(when (memq window-system '(mac ns)) (exec-path-from-shell-initialize))
(setq exec-path-from-shell-check-startup-files nil)

;; hungry delete package config
;; (require 'hungry-delete)
;; (global-hungry-delete-mode)

;; swiper config
(straight-use-package 'swiper)
(use-package swiper
  :ensure t

  )
(straight-use-package 'ivy)
(use-package ivy
  :ensure t
  :bind (
         "C-c C-r" . 'ivy-resume
         )
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )
(straight-use-package 'ivy-xref)
(straight-use-package 'ivy-hydra)

;; smartparens config
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(straight-use-package 'smartparens)
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  )

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; js files config
(setq auto-mode-alist
      (append
       '(
	     ("\\.js\\'" . js2-mode)
	     ("\\.html\\'" . web-mode)
	     )
       auto-mode-alist))

;; web-mode
(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	    (setq js-indent-level (if (= js-indent-level 2) 4 2))
	    (setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	         (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	         (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

(global-set-key (kbd "C-c t i") 'my-toggle-web-indent)

;; js2-refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

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

;; theme
(load-theme 'srcery t)

;; popwin config
(use-package popwin
  :ensure t
  :config
  (popwin-mode t)
  )

;; counsel-imenu
(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			                   ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
			                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			                   ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
			                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
			                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
	      (lambda ()
	        (setq imenu-create-index-function 'js2-imenu-make-index)))

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
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; auto yasnippet
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

;; evil mode
(straight-use-package 'evil)
(use-package evil
  :ensure t
  :init
  (setq
   evil-insert-state-cursor 'bar
   evil-normal-state-cursor 'box
   evil-emacs-state-cursor 'bar
   )
  :config
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  )

;; undo and redo
(straight-use-package 'undo-tree)
(use-package undo-tree
  :ensure t
  :config
  (turn-on-undo-tree-mode)
  ;; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  )

;; surround
(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))

;; commenter
(use-package evil-leader
  :ensure t
  :after (evil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "sf" 'counsel-rg
    ;; "ff" 'find-file
    "ff" 'counsel-git
    "bb" 'switch-to-buffer
    "w/" 'split-window-right
    "w-" 'split-window-below
    ":"  'counsel-M-x
    "wM" 'delete-other-windows
    )
  )
;; evil nerd commenter
(use-package evil-nerd-commenter
  :ensure t
  :config
  (evil-leader/set-key
    "cc" 'evilnc-comment-or-uncomment-lines
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;; "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator
    )
  )
(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
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

;; ;; neotree, sidebar explorer
;; (use-package neotree
;;   :ensure t
;;   :config
;;   (evil-leader/set-key
;;     "ee" 'neotree-toggle
;;     )
;;   (add-hook 'neotree-mode-hook
;; 	        (lambda ()
;; 	          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;; 	          (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
;; 	          (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
;; 	          (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
;; 	          (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
;; 	          (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
;; 	          (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
;; 	          (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
;; 	          (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)
;; 	          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
;; 	          )
;; 	        )
;;   )

;; treemacs, sidebar explorer
(use-package treemacs
  :ensure t
  :defer t
  :config
  (evil-leader/set-key
    "ee" 'treemacs
    )
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
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; jump window like tmux prefix-q
(use-package switch-window
  :ensure t
  :config
  ;; (setq switch-window-shortcut-appearance 'asciiart)
  (evil-leader/set-key
    "qq" 'switch-window
    ))


;; powerline-evil
(use-package powerline-evil
  :ensure t
  )

;; golang
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))
  )

(use-package go-tag
  :ensure t
  :after go-tag go-mode
  )

;; json
(use-package json-mode)

;; yaml
(use-package yaml-mode)

;; pb
(use-package protobuf-mode)

;; lsp
(use-package lsp-mode
  :ensure t
  :hook (
         (go-mode . lsp)
         )
  :commands lsp
  )

;; jump to declearation
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

;; nyan cat
(use-package nyan-mode
  :ensure t
  :init (setq
         nyan-animate-nyancat t
  	     nyan-bar-length 10
         )
  :hook ((after-init . nyan-mode)))

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
(straight-use-package 'dashboard)
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

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (setq
   evil-emacs-state-cursor 'bar
   evil-insert-state-cursor 'bar
   evil-normal-state-cursor 'box
   )
  :config
  (evil-terminal-cursor-changer-activate)
  )

(use-package ace-jump-mode
  :ensure t
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

(provide 'init-packages)
;;; init-packages.el ends here
