(add-to-list 'load-path "~/.emacs.d/config/")

(require 'init-packages)

(setq ring-bell-function 'ignore)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; e(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


;; org mode
;; org code blocks highlight
(require 'org)
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)

;; --- none pacakge config ---

;; abbreviation
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table
  '(
    ("abbr" "abbreviation")
    ))

;; hide tool bar
(tool-bar-mode -1)
;; hide scroll bar
(scroll-bar-mode -1)
;; skip startup page
(setq inhibit-splash-screen t)

;; show line number
(global-linum-mode t)

;; open config file command
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; show recent files
(recentf-mode t)


;; cursor style
(setq-default cursor-type 'bar)

;; no backup files
(setq make-backup-files nil)

;; disable autosave
(setq auto-save-default nil)



;; recent file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; delete selected content
(delete-selection-mode t)

;; fullscreen startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; highlight matching brackets
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; highlight current line
(global-hl-line-mode t)



;; find source
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; auto load outer change
(global-auto-revert-mode t)

;; package management
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.01)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   '("8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
