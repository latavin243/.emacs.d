;; auto update packages
(use-package auto-package-update
   :ensure t
   :config
   ;; (setq auto-package-update-delete-old-versions t)
   (setq auto-package-update-interval 4)
   (auto-package-update-maybe))

;; ensure system package
(use-package use-package-ensure-system-package)

(provide 'init-package-management)
