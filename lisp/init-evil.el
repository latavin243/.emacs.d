;;; init-evil.el

;; evil mode
(use-package evil
  :ensure t
  :after (undo-tree)
  :init
  (setq
   ;; evil-insert-state-cursor 'bar
   ;; evil-normal-state-cursor 'box
   ;; evil-emacs-state-cursor 'bar
   )
  :config
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (evil-set-undo-system 'undo-tree)

  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

  (evil-define-key 'normal 'global
    (kbd "C-u") 'evil-scroll-up

    "[b" 'previous-buffer
    "]b" 'next-buffer

    "u" 'undo-tree-undo
    (kbd "C-r") 'undo-tree-redo
  )
  ;; (evil-set-initial-state 'org-mode 'emacs)
  )

;; leader
(use-package evil-leader
  :ensure t
  :after (evil counsel fzf projectile)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "sf" 'counsel-rg
    "rg" 'counsel-rg
    ;; "ff" 'fzf
    ;; "ff" 'counsel-git
    ;; "ff" 'counsel-find-file
    "ff" 'projectile-find-file
    ;; "bb" 'switch-to-buffer
    "bb" 'counsel-switch-buffer
    "w/" 'split-window-right
    "w-" 'split-window-below
    ":"  'counsel-M-x
    "wM" 'delete-other-windows
    "nn" 'narrow-to-region
    "nw" 'widen
    "nf" 'narrow-to-defun
    "lf" 'imenu

    ;; commenter
    "cc" 'evilnc-comment-or-uncomment-lines
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    ;; "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    ;; "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator

    ;; string-inflection
    "rs" 'string-inflection-underscore
    "rc" 'string-inflection-lower-camelcase
    "rC" 'string-inflection-camelcase
    "rp" 'string-inflection-camelcase
    "rk" 'string-inflection-kebab-case
    )
  )

;; surround
(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1)
  )

;; like vim-abolish, convert from camelCase to snake_case
(use-package string-inflection
  :ensure t
  :after (evil)
  )

;; commenter
(use-package evil-nerd-commenter
  :ensure t
  :after (evil)
  )

;; (use-package evil-terminal-cursor-changer
;;   :ensure t
;;   :init
;;   (setq
;;    evil-emacs-state-cursor 'bar
;;    evil-insert-state-cursor 'bar
;;    evil-normal-state-cursor 'box
;;    )
;;   :config
;;   (evil-terminal-cursor-changer-activate)
;;   )

;; (use-package evil-magit
;;   :ensure t
;;   :after (evil magit)
;;   )

;; powerline-evil
(use-package powerline-evil
  :ensure t
  )

(provide 'init-evil)
;;; init-evil.el ends here
