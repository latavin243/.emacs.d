;;; package management
(require 'package)
(package-initialize)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			             ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; cl - Common Lisp Extension
(require 'cl)
(setq byte-compile-warnings '(cl-functions))

;; Add Packages
(defvar my/packages
  '(
    ;; --- package management ---
    use-package
    ;; --- Auto-completion ---
    company
    ;; --- Better Editor ---
    ;; hungry-delete
    swiper
    counsel
    smartparens
    popwin
    expand-region
    iedit
    ;; --- Major Mode ---
    js2-mode
    ;; --- Minor Mode ---
    nodejs-repl
    exec-path-from-shell
    js2-refactor
    ;; --- web ---
    web-mode
    ;; --- Themes ---
    dakrone-theme
    ;; --- Others ---
    reveal-in-osx-finder
    org-pomodoro
    helm-ag
    yasnippet
    auto-yasnippet
    ;; --- vim ---
    window-numbering
    powerline-evil
    evil-nerd-commenter
    which-key
    undo-tree
    ) "Custom packages.")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; exec path from shell
;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq exec-path-from-shell-check-startup-files nil)

;; hungry delete package config
;; (require 'hungry-delete)
;; (global-hungry-delete-mode)

;; swiper config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)

;; smartparens config
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)

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
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; theme
(load-theme 'dakrone t)

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
(global-set-key (kbd "C-=") 'er/expand-region)

;; iedit
;; (global-set-key (kbd "M-s e") 'iedit-mode)
;; c-; will do

;; helm-ag
(global-set-key (kbd "C-c p s") 'helm-do-ag-project-root)

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
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  )

;; surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; commenter
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
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
;; undo and redo
(use-package undo-tree
  :ensure t
  :config
  (turn-on-undo-tree-mode)
  )
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

;; neotree
(use-package neotree
  :ensure t
  :config
  (evil-leader/set-key
    "ee" 'neotree-toggle
    )
  (add-hook 'neotree-mode-hook
	        (lambda ()
	          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	          (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
	          (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
	          (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
	          (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
	          (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
	          (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

	          (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
	          (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

	          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	          )
	        )
  )

;; window numbering
(window-numbering-mode 1)

;; ace window, jump window
;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (evil-leader/set-key
;;     "qq" 'ace-window
;;     ))

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
  :ensure t)

;; golang
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; lsp
(use-package lsp-mode
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
  	     nyan-bar-length 16
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

;; doom modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

;; startup dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Happy Hacking!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  ;;(setq dashboard-show-shortcuts nil)
  (setq dashboard-items '(
                          (recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          ;;(agenda . 5)
                          ;;(registers . 5)
                          ))
  )

;; file fin
(provide 'init-packages)
