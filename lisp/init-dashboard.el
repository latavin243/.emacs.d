;; dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Happy Hacking")
  (setq dashboard-startup-banner nil) ;; can be 'official, etc
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

(provide 'init-dashboard)
