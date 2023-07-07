(if (display-graphic-p)
    (progn
      ;; hide tool-bar and scroll-bar
      (tool-bar-mode -1)
      (scroll-bar-mode -1)

      ;; skip startup page
      (setq inhibit-splash-screen t)

      ;; cursor style
      (setq-default cursor-type 'bar)

      ;; fullscreen startup
      (setq initial-frame-alist (quote ((fullscreen . maximized))))

      ;; highlight current line
      (global-hl-line-mode t)
      ))

;; theme
(use-package srcery-theme)
(load-theme 'srcery t)

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

(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

;; doom modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'init-apperance)
