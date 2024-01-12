;;; init-package.el -- package management
;;; Commentary:
;;; package config
;;; Code:

;; important packages
;; others
(use-package exec-path-from-shell)
(use-package reveal-in-osx-finder)

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
(use-package iedit)
;; (global-set-key (kbd "M-s e") 'iedit-mode)
;; c-; will do

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  )

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

;; (use-package company-lsp
;;   :ensure t
;;   :defer t
;;   :init (setq company-lsp-cache-candidates 'auto))

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
